module rec SecurityService

open System
open System.Security.Claims
open System.Text
open System.IdentityModel.Tokens.Jwt
open Microsoft.AspNetCore.Http
open Microsoft.IdentityModel.Tokens
open Giraffe.SerilogExtensions
open Serilog
open Microsoft.Extensions.Configuration

open Shared
open Common
open Domain

module AR = AsyncResult

module CustomClaims =
    let Name = "name"
    let Role = "role'"
    let TeamId = "teamId"
    let QuizId = "quizId"
    let SessionId = "sessionId"
    let UserName = "username"
    let UserId = "userId"

module CustomRoles =
    let Admin = "admin"
    let Expert = "expert"
    let Team = "team"
    let Reg = "reg"
    let Aud = "aud"

let rand = Random(DateTime.UtcNow.Millisecond)

let private generateToken (secret:string) claims =
    let key = SymmetricSecurityKey <| Encoding.ASCII.GetBytes secret

#if DEBUG
    let expires = DateTime.UtcNow.AddMinutes 1.0
#else
    let expires = DateTime.UtcNow.AddHours 12.0
#endif

    let jwt = JwtSecurityToken(
                issuer = "open-quiz",
                audience = "any",
                claims = claims,
                expires = Nullable expires,
                signingCredentials = SigningCredentials(key, SecurityAlgorithms.HmacSha256)
    )

    JwtSecurityTokenHandler().WriteToken jwt

let getClaimsFromToken (token:string) =
    if String.IsNullOrEmpty token then
        Map.empty
    else
        try
            let jwt = JwtSecurityTokenHandler().ReadJwtToken token
            jwt.Claims
            |> Seq.map (fun claim -> claim.Type, claim.Value)
            |> Map.ofSeq
        with
        | ex -> [("ERROR", ex.Message)] |> Map.ofList

let private validateJwt (secret:string) (jwt : string) validateLifetime  =
    let key = SymmetricSecurityKey (Encoding.ASCII.GetBytes secret)

    let validationParameters = TokenValidationParameters (
                                ValidateAudience = false,
                                ValidateIssuer = false,
                                ValidateIssuerSigningKey = true,
                                IssuerSigningKey = key,
                                ValidateLifetime = validateLifetime
    )

#if DEBUG
    validationParameters.ClockSkew <- TimeSpan.Zero
#endif

    let tokenHandler = JwtSecurityTokenHandler()

    try
        let principal, securityToken = tokenHandler.ValidateToken (jwt, validationParameters)

        let jwtToken = securityToken :?> JwtSecurityToken
        if jwtToken.Header.Alg.Equals(SecurityAlgorithms.HmacSha256, StringComparison.InvariantCultureIgnoreCase) then
            Ok principal
        else
            Error SecurityTokenInvalid
    with
        | :? SecurityTokenExpiredException -> Error SecurityTokenExpired
        | _ -> Error SecurityTokenInvalid

let private createRefreshToken () =
    { Value = generateRandomToken(); Expired = DateTime.UtcNow.AddDays 7.0} : RefreshToken

let refreshToken secret (req:REQ<{| RefreshToken: string |}>) =
    async{
        let status, value =
            match validateJwt secret req.Token false with
            | Ok principal ->
                let res =
                    async{
                        match! Data2.RefreshTokens.get req.Arg.RefreshToken with
                        | Ok refreshToken when refreshToken.Expired < DateTime.UtcNow -> return Error "Refresh token expired"
                        | Ok refreshToken ->
                            let newRefreshToken = createRefreshToken()
                            match! Data2.RefreshTokens.replace refreshToken newRefreshToken with
                            | Ok _ ->
                                let token = generateToken secret principal.Claims
                                return Ok {|Token = token; RefreshToken = newRefreshToken.Value|}
                            | Error _ -> return Error "Replace Token Error"
                        | Error _ -> return Error "Refresh token not found"
                    }
                Executed, res
            | Error status -> status, async {return Error (status.ToString() )}

        let! x = value
        return {Status = status; Value = x; ST = DateTime.UtcNow}
    }

let authorize secret role (f : ClaimsPrincipal -> 'Arg -> ARES<'Value>) : REQ<'Arg> -> ARESP<'Value> =
    fun req ->
        let status, res =
            match validateJwt secret req.Token true with
            | Ok principal when principal.FindFirstValue CustomClaims.Role = role -> Executed, Ok principal
            | Ok _ -> Unauthorized, Error (Unauthorized.ToString())
            | Error status -> status, Error (status.ToString())

        async{
            let! value =
                match res with
                | Ok principal -> async { return! f principal req.Arg }
                | Error txt -> async { return Error txt }

            return {Status = status; Value = value; ST = DateTime.UtcNow}
        }

let authorizeExpert secret (f: string -> string -> 'arg -> ARES<'res>) =
    authorize secret CustomRoles.Expert <| fun principal req ->
        let sub = principal.FindFirstValue CustomClaims.UserId
        let username = principal.FindFirstValue CustomClaims.UserName
        f sub username req

let authorizeExpertCheckPrivateQuiz secret (f: string -> string -> string -> string -> 'arg -> ARES<'res>) =
    authorize secret CustomRoles.Expert <| fun principal req ->
        let sub = principal.FindFirstValue CustomClaims.UserId
        let usernamesys = principal.FindFirstValue CustomClaims.UserName
        let username = principal.FindFirstValue CustomClaims.Name
        let quizIdStr = principal.FindFirstValue CustomClaims.QuizId

        f sub usernamesys username quizIdStr req

let authorizeAdmin secret (f: string -> 'arg -> ARES<'res>) =
    authorize secret CustomRoles.Admin <| fun principal req ->
        let quizIdStr = principal.FindFirstValue CustomClaims.QuizId
        f quizIdStr req

let private teamKey (principal : ClaimsPrincipal) =
    let quizIdStr = principal.FindFirstValue CustomClaims.QuizId
    let teamIdStr = principal.FindFirstValue CustomClaims.TeamId

    match (Int32.TryParse quizIdStr),(Int32.TryParse teamIdStr) with
    | (true, quizId), (true, teamId) -> Some {QuizId = quizId; TeamId = teamId}
    | _ -> None

let private teamSessionId (principal : ClaimsPrincipal) =
    let sessionIdStr = principal.FindFirstValue CustomClaims.SessionId
    match Int32.TryParse sessionIdStr with
    | (true, sessionId) -> Some sessionId
    | _ -> None

let authorizeTeam secret (f: int -> TeamKey -> 'arg -> ARES<'res>) =
    authorize secret CustomRoles.Team <| fun principal req ->
        result {
            let! teamKey = (teamKey principal, "Invalid team's key")
            let! sessionId = (teamSessionId principal, "Wrong session Id")
            return (sessionId, teamKey)
        }
        |> AsyncResult.fromResult
        |> AsyncResult.bind (fun (sessionId, teamKey) -> f sessionId teamKey req)

let authorizePrivateReg secret (f: string -> 'arg -> ARES<'res>) =
    authorize secret CustomRoles.Reg <| fun principal req ->
        let quizIdStr = principal.FindFirstValue CustomClaims.QuizId
        f quizIdStr req

let authorizeAudience secret (f: string -> 'arg -> ARES<'res>) =
    authorize secret CustomRoles.Aud <| fun principal req ->
        let quizIdStr = principal.FindFirstValue CustomClaims.QuizId
        f quizIdStr req

let execute (logger : ILogger) (proc:string) (f: REQ<'Req> -> RESP<'Resp>) : REQ<'Req> -> ARESP<'Resp> =
    fun req ->
        async {
            let claims = getClaimsFromToken req.Token
            logger.Information("{@Op} {@Proc} {@Arg} {@Claims}", "Exec", proc, req.Arg, claims)
            let resp = f req
            logger.Information("{@Op} {@Proc} {@ServerResponse}", "Done", proc, resp)
            return resp
        }

let exec (logger : ILogger) (proc:string) (f: REQ<'Req> -> ARESP<'Resp>) : REQ<'Req> -> ARESP<'Resp> =
    fun req ->
        async {
            let claims = getClaimsFromToken req.Token
            logger.Information("{@Op} {@Proc} {@Arg} {@Claims}", "Exec", proc, req.Arg, claims)
            let! resp = f req
            logger.Information("{@Op} {@Proc} {@ServerResponse}", "Done", proc, resp)
            return resp
        }

let loginResp secret claims user =
    async{
        let token = generateToken secret claims
        let refreshToken = createRefreshToken()

        let! res = Data2.RefreshTokens.put refreshToken
        return res |> Result.map (fun _ -> {|Token = token; RefreshToken = refreshToken.Value; User = user|})
    }

let private tryExtractPrivateQuizId secret token =
    if token <> "" then
        match validateJwt secret token true with
        | Ok principal when principal.FindFirstValue CustomClaims.Role = CustomRoles.Reg ->
            let quizIdStr = principal.FindFirstValue CustomClaims.QuizId
            match Int32.TryParse quizIdStr with
            | true, quizId -> Some quizId
            | _ -> None
        | _ -> None
    else None

let loginMainUser secret token clientId clientName redirectUrl code =
    async {
        let! tokensResult = Aws.getUserToken clientName clientId redirectUrl code
        match tokensResult with
        | Ok tokens ->
            let! userInfoResult = Aws.getUserInfo clientName tokens.AccessToken
            match userInfoResult with
            | Ok info ->
                let privateQuizId = tryExtractPrivateQuizId secret token
                let! exp = Data2.Experts.get info.Sub

                // update expert if attributes changed
                let! _ =
                    match exp with
                    | Ok exp when exp.Username <> info.Username || exp.Name <> info.Name ->
                        let logic (exp:Domain.Expert) = {exp with Username = info.Username; Name = info.Name} |> Ok
                        Data2.Experts.update exp.Id logic
                        |> AsyncResult.map ignore
                    | _-> AsyncResult.retn ()

                let user = MainUser {
                        Sub = info.Sub
                        Username = info.Username
                        Name = info.Name
                        PictureUrl = info.Picture
                        IsProducer = match exp with Ok exp -> exp.IsProducer | _ -> false
                        IsPrivate = privateQuizId.IsSome
                }

                let claims = [
                    Claim(CustomClaims.UserId, info.Sub)
                    Claim(CustomClaims.UserName, info.Username)
                    Claim(CustomClaims.Name, info.Name)
                    Claim(CustomClaims.Role, CustomRoles.Expert)
                    if (privateQuizId.IsSome) then Claim(CustomClaims.QuizId, privateQuizId.Value.ToString())
                ]
                return! loginResp secret claims user
            | Error txt ->
                return Error txt
        | Error txt ->
            return Error txt
    }

let loginAdminUser secret quizId token =
    Data2.Quizzes.getDescriptor quizId
    |> AR.bind (fun quiz ->
        if quiz.AdminToken = System.Web.HttpUtility.UrlDecode token then
            let claims = [Claim(CustomClaims.Role, CustomRoles.Admin); Claim(CustomClaims.QuizId, quiz.QuizId.ToString())]
            let user = AdminUser {QuizId = quiz.QuizId; QuizName = quiz.Name; QuizImg = quiz.ImgKey}
            loginResp secret claims user
        else Error "Wrong entry token" |> AR.fromResult)

let loginTeamUser secret quizId teamId token =
    Data2.Teams.getDescriptor quizId teamId
    |> AR.bind (fun team ->
        if team.EntryToken = token then
            Data2.Quizzes.getDescriptor quizId
            |> AR.bind (fun quiz ->
                let sessionId = rand.Next(Int32.MaxValue)
                let user = {QuizId = team.QuizId; QuizName = quiz.Name; TeamId = team.TeamId; TeamName = team.Name}
                let claims = [
                    Claim(CustomClaims.Name, user.TeamName)
                    Claim(CustomClaims.Role, CustomRoles.Team)
                    Claim(CustomClaims.QuizId, user.QuizId.ToString())
                    Claim(CustomClaims.TeamId, user.TeamId.ToString())
                    Claim(CustomClaims.SessionId, sessionId.ToString())]
                if team.ActiveSessionId = 0 then
                    Data2.Teams.update {QuizId = quizId; TeamId = teamId}
                      (fun team -> team |> Domain.Teams.dsc (fun dsc -> {dsc with ActiveSessionId = sessionId} |> Ok))
                    |> AR.next (AR.retn ())
                else AR.retn ()
                |> AR.next (loginResp secret claims (TeamUser user)))
        else Error "Authenticaton Error" |> AR.fromResult)

let loginRegUser secret quizId token =
    Data2.Quizzes.getDescriptor quizId
    |> AR.bind (fun quiz ->
        if quiz.RegToken = System.Web.HttpUtility.UrlDecode token then
            let claims = [Claim(CustomClaims.Role, CustomRoles.Reg); Claim(CustomClaims.QuizId, quiz.QuizId.ToString())]
            let user = RegUser {QuizId = quiz.QuizId}
            loginResp secret claims user
        else Error "Wrong entry token" |> AR.fromResult)

let loginAudUser secret quizId token =
    Data2.Quizzes.getDescriptor quizId
    |> AR.bind (fun quiz ->
        if quiz.ListenToken = System.Web.HttpUtility.UrlDecode token then
            let claims = [Claim(CustomClaims.Role, CustomRoles.Aud); Claim(CustomClaims.QuizId, quiz.QuizId.ToString())]
            let user = RegUser {QuizId = quiz.QuizId}
            loginResp secret claims user
        else Error "Wrong entry token" |> AR.fromResult)

let api (context:HttpContext) : ISecurityApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret = Config.getJwtSecret cfg

    let api : ISecurityApi = {
        login = exec logger "login" <| executedResponse  (login secret cfg)
        refreshToken = exec logger "refreshToken" <| refreshToken secret
    }

    api

let login secret (cfg:IConfiguration) (token:string) (req : LoginReq) =
    match req with
    | LoginReq.MainUser data ->
        let clientId = Config.getCognitoClientId cfg
        let clientName = Config.getCognitoClientName cfg
        let redirectUri = Config.getRedirectUrl cfg
        loginMainUser secret token clientId clientName redirectUri data.Code
    | LoginReq.AdminUser data -> loginAdminUser secret data.QuizId data.Token
    | LoginReq.TeamUser data -> loginTeamUser secret data.QuizId data.TeamId data.Token
    | LoginReq.RegUser data -> loginRegUser secret data.QuizId data.Token
    | LoginReq.AudUser data -> loginAudUser secret data.QuizId data.Token