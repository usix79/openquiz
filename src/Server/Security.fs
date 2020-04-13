module rec Security

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


module CustomClaims =
    let Name = "name"
    let Role = "role'"
    let TeamId = "teamId"
    let QuizId = "quizId"
    let SessionId = "sessionId"

module CustomRoles =
    let Admin = "admin"
    let Competitor = "competitor"
    let Expert = "expert"


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
    let status, value =
        match validateJwt secret req.Token false with
        | Ok principal ->
            let res =
                match Data.RefreshTokens.get req.Arg.RefreshToken with
                | Some refreshToken when refreshToken.Expired < DateTime.UtcNow -> Error "Refresh token expired"
                | Some refreshToken ->
                    let newRefreshToken = createRefreshToken()
                    Data.RefreshTokens.replace refreshToken newRefreshToken
                    let token = generateToken secret principal.Claims
                    Ok {|Token = token; RefreshToken = newRefreshToken.Value|}
                | None -> Error "Refresh token not found"
            Executed, res
        | Error status -> status, Error (status.ToString())

    {Status = status; Value = value; ST = DateTime.UtcNow}



// let private loginWithToken secret oldSessionId (args:{| GameId: int; TeamId: int; EntryToken: string |}) =

//     let team = Data.getTeam args.GameId args.TeamId
//     let game = Data.getGame args.GameId

//     match game, team with
//     | Some game, Some team ->
//         let sessionId =
//             match oldSessionId with
//             | Some id -> id
//             | None -> rand.Next(Int32.MaxValue)

//         let takeActiveSessionResult =
//             if team.ActiveSessionId = sessionId then Ok true
//             else if team.ActiveSessionId = 0 then
//                 match Data.updateTeam {team with ActiveSessionId = sessionId} with
//                 | Ok _ -> Ok true
//                 | Error txt -> Error txt
//             else Ok false

//         match takeActiveSessionResult with
//         | Ok isActive ->
//             let user = {GameId = team.GameId; GameName = game.Name; TeamId = team.Id; TeamName = team.Name}

//             let token = generateToken secret [
//                 Claim(CustomClaims.Name, user.TeamName)
//                 Claim(CustomClaims.Role, CustomRoles.Team)
//                 Claim(CustomClaims.GameId, user.GameId.ToString())
//                 Claim(CustomClaims.TeamId, user.TeamId.ToString())
//                 Claim(CustomClaims.SessionId, sessionId.ToString())
//             ]

//             let refreshToken = createRefreshToken()
//             Data.addRefreshToken refreshToken

//             Ok {|Token = token; RefreshToken = refreshToken.Value; User = TeamUser user|}
//         | Error txt -> Error txt
//     | _ -> Error "authenticaton error"

let authorize secret role (f : ClaimsPrincipal -> 'Arg -> Result<'Value, string>) : REQ<'Arg> -> RESP<'Value> =
    fun req ->
        let status, res =
            match validateJwt secret req.Token true with
            | Ok principal when principal.FindFirstValue CustomClaims.Role = role -> Executed, f principal req.Arg
            | Ok _ -> Unauthorized, Error (Unauthorized.ToString())
            | Error status -> status, Error (status.ToString())

        {Status = status; Value = res; ST = DateTime.UtcNow}

let authorizeExpert secret (f: string -> 'arg -> Result<'res, string>) =
    authorize secret CustomRoles.Expert <| fun principal req ->
        let subStr = principal.FindFirstValue CustomClaims.Name
        f subStr req

// let competitorKey (principal : ClaimsPrincipal) =
//     let quizIdStr = principal.FindFirstValue CustomClaims.QuizId
//     let compIdStr = principal.FindFirstValue CustomClaims.CompetitorId

//     match Int32.TryParse quizIdStr with
//     | (true, quizId) ->
//         match Int32.TryParse compIdStr with
//         | (true, compId) -> Some {|QuizId = quizId; CompetirorId = compId|}
//         | _ -> None
//     | _ -> None

// let competitorSessionId (principal : ClaimsPrincipal) =
//     let sessionIdStr = principal.FindFirstValue CustomClaims.SessionId
//     match Int32.TryParse sessionIdStr with
//     | (true, sessionId) -> Some sessionId
//     | _ -> None

// let authorizeAdmin secret f =
//     authorize secret CustomRoles.Admin f

// let authorizeTeamWithSession secret (f: int -> Team -> 'arg -> Result<'res, string>) =
//     authorize secret CustomRoles.Team <| fun principal req ->
//         match teamKey principal with
//         | Some key ->
//             let sessionId = teamSessionId principal
//             match Data.getTeam key.GameId key.TeamId with
//             | Some team ->
//                 match sessionId with
//                 | Some sessionId -> f sessionId team req
//                 | _ -> Error "Wrong session Id"
//             | None -> Error "Team not found"
//         | None -> Error "Invalid team's key"



// let authorizeTeam secret (f: Team -> 'arg -> Result<'res, string>) =
//     authorizeTeamWithSession secret <| fun sessionId team req ->
//         if team.ActiveSessionId = sessionId then f team req
//         else Error Errors.SessionIsNotActive



let execute (logger : ILogger) (proc:string) (f: REQ<'Req> -> RESP<'Resp>) : REQ<'Req> -> ARESP<'Resp> =

    fun req ->
        async {
            let claims = getClaimsFromToken req.Token
            logger.Information("{@Op} {@Proc} {@Arg} {@Claims}", "Exec", proc, req.Arg, claims)
            let resp = f req
            logger.Information("{@Op} {@Proc} {@ServerResponse}", "Done", proc, resp)
            return resp
        }

// let extractSessionId (f: int option -> 'Req -> Result<'Resp, string>) : ServerRequest<'Req> -> ServerResponse<'Resp> =

//     fun req ->
//         let claims = getClaimsFromToken req.Token
//         let sessionId =
//             match claims.TryFind CustomClaims.SessionId with
//             | Some strId ->
//                 match Int32.TryParse strId with
//                 | true, id -> Some id
//                 | _ -> None
//             | _ -> None

//         serverResponse (fun arg -> f sessionId arg) req

let loginResp secret claims user =
    let token = generateToken secret claims
    let refreshToken = createRefreshToken()
    Data.RefreshTokens.add refreshToken
    Ok {|Token = token; RefreshToken = refreshToken.Value; User = user|}

let loginMainUser secret clientId clientName redirectUrl code =
    async {
        let! tokensResult = Aws.getUserToken clientName clientId redirectUrl code
        match tokensResult with
        | Ok tokens ->
            let! userInfoResult = Aws.getUserInfo clientName tokens.AccessToken
            match userInfoResult with
            | Ok userInfo ->
                let isProducer = match Data.Experts.get userInfo.Sub with Some exp -> exp.IsProducer | None -> false
                let user = MainUser {Sub = userInfo.Sub; Name = userInfo.Name; PictureUrl = userInfo.Picture; IsProducer = isProducer}
                let claims = [Claim(CustomClaims.Name, userInfo.Sub); Claim(CustomClaims.Role, CustomRoles.Expert)]
                return loginResp secret claims user
            | Error txt ->
                return Error txt
        | Error txt ->
            return Error txt
    } |> Async.RunSynchronously

let loginAdminUser secret quizId token =
    result{
        let! quiz = ((Data.Quizzes.getDescriptor quizId), "Quiz not found")

        return!
            if quiz.AdminToken = token then
                let claims = [Claim(CustomClaims.Role, CustomRoles.Admin); Claim(CustomClaims.QuizId, quiz.QuizId.ToString())]
                let user = AdminUser {QuizId = quiz.QuizId; QuizName = quiz.Name}
                loginResp secret claims user
            else
                printfn "%s" quiz.AdminToken
                printfn "%s" token
                Error "Wrong entry token"
    }

let securityApi (context:HttpContext) : ISecurityApi =
    let logger : ILogger = context.Logger()
    let cfg = context.GetService<IConfiguration>()
    let secret = Config.getJwtSecret cfg

    let api : ISecurityApi = {
        login = execute logger "login" <| executedResponse  (login secret cfg)
        refreshToken = execute logger "refreshToken" <| refreshToken secret
    }

    api

let login secret (cfg:IConfiguration) (req : LoginReq) =
    match req with
    | LoginReq.MainUser data ->
        let clientId = Config.getCognitoClientId cfg
        let clientName = Config.getCognitoClientName cfg
        let redirectUri = Config.getRedirectUrl cfg
        loginMainUser secret clientId clientName redirectUri data.Code
    | LoginReq.AdminUser data ->
        loginAdminUser secret data.QuizId data.Token
