module L10n

type Languages =
    | English
    | Russian
    | Ukrainian
    | Azerbaijanian
    | Uzbek

type CommonL10n = {
    Question : string
    Answer : string
    Comment : string
    PointsSmall : string
    Team : string
    QuizIsNotStarted : string
    QuizIsNotFinished : string
    MenuHistory : string
    MenuQuestion : string
    MenuResults : string
    MenuLastCall : string
}

type TeamL10n = {
    Common : CommonL10n
    NotActive: string
    AnotherSession : string
    UseThisDevice : string
    YourAnswer: string
    WaitingForConfirmation : string
    RegistrationHasBeenRejected : string
    Jeopardy : string
    Question : string
    Answer : string
    Points : string
    Vote : string
    CorrectAnswer : string
}

type AudienceL10n = {
    Common : CommonL10n
    Audience : string
    Question : string
    CorrectAnswer : string
    CorrectAnswers : string
}

type RegL10n = {
    Details : string
    Procceed : string
    Registration : string
    Register : string
    RegisteredAs : string
    Status : string
    Pending : string
    Confirmed : string
    Rejected : string
    Edit : string
    Enter : string
    LinkForAudience : string
    TeamName : string
}

let private l10n lang en ru ua az uz =
    match lang with
    | English -> en
    | Russian -> ru
    | Ukrainian -> ua
    | Azerbaijanian -> az
    | Uzbek -> uz

let commonL10n lang = {
    Question = l10n lang "Question" "Вопрос" "Питання" "Sual" "Савол"
    Answer = l10n lang "Answer" "Ответ" "Відповідь" "Cavab" "Жавоб"
    Comment = l10n lang "Comment" "Комментарий" "Коментар" "Şərh" "Изоҳ"
    PointsSmall = l10n lang "points" "очка" "очки" "xallar" "очко"
    Team = l10n lang "Team" "Команда" "Команда" "Komanda" "Жамоа"
    QuizIsNotStarted = l10n lang "Quiz is not started" "Квиз еще не начался" "Квіз ще не почався" "Oyun Başlamayıb" "Ўйин ҳали бошланмаган"
    QuizIsNotFinished = l10n lang "Quiz is finished" "Квиз завершен" "Квіз завершено" "Oyun Bitdi" "Ўйин тугади"
    MenuQuestion = l10n lang "Question" "Вопрос" "Питання" "Sual" "Савол"
    MenuHistory = l10n lang "History" "История" "Історія" "Cavablar" "Тарих"
    MenuResults = l10n lang "Results" "Итоги" "Підсумки" "Nəticələr" "Натижалар"
    MenuLastCall = l10n lang "Last Call" "Время" "Час" "Son saniyələr" "Вақт"
}

let teamL10n lang = {
    Common = commonL10n lang
    NotActive = l10n lang "NOT ACTIVE" "НЕАКТИВНО" "НЕ АКТИВНО" "AKTİV DEYİL" "ФАОЛ ЭМАС"
    AnotherSession = l10n lang "Team's session is running from another device. Only one device is allowed per team." "Капитанская сессия запущена на другом устройстве. Допускается лишь одна активная сессия в команде." "Капітанська сесія запущена на іншому пристрої. Допускається лише одна активна сесія в команді." "Komanda kapitanlığı başqa cihazda aktivdir. Hər komanda üçün yalnız bir cihaza icazə verilir. Kapitanlığı qəbul etmək üçün Aktiv edin." "Жамоа сардорининг ҳаволаси бошқа мосламада очилган. Ҳар жамоадан фақат битта фаол сессия бўлиши мумкин"
    UseThisDevice = l10n lang "Activate" "Активировать" "Активувати" "Aktivləşdirin" "Фаоллаштириш"
    YourAnswer = l10n lang "Your answer" "Ваш ответ" "Ваша відповідь" "Sənin cavabın" "Жавобингиз"
    WaitingForConfirmation = l10n lang "Waiting for confirmation of the registration..." "Ожидается подтверждение регистрации..." "Очікується підтвердження реєстрації..." "Qeydiyyatınız təsdiq edilir..." "Рўйхатдан ўтганингиз тасдиқланмоқда"
    RegistrationHasBeenRejected = l10n lang "Registration has been rejected (" "Регистрация была отклонена (" "Реєстрація була відхилена (" "Qeydiyyatınız rədd elildi (" "Рўйхатдан ўтиш рад этилди ("
    Jeopardy = l10n lang "Jeopardy!" "Рискую!" "Ризикую!" "Jeopardy!" "Таваккал!"
    Question = l10n lang "Question" "Вопрос" "Питання" "Sual" "Савол"
    Answer = l10n lang "Answer" "Ответ" "Відповідь" "Cavab" "Жавоб"
    Points = l10n lang "Points" "Очки" "Очки" "Xal" "Очко"
    Vote = l10n lang "Rate the question" "Оцените вопрос" "Оцініть питання" "Sualı qiymətləndirin" "Саволга баҳо беринг"
    CorrectAnswer = l10n lang "correct answer" "правильный ответ" "правильна відповідь" "doğru cavab" "Тўғри жавоб"
}

let audienceL10n lang = {
    Common = commonL10n lang
    Audience = l10n lang "audience" "зрители" "глядачі" "izləyici" "Муҳлисслар"
    Question = l10n lang "Question" "Вопрос" "Питання" "Sual" "Савол"
    CorrectAnswer = l10n lang "correct answer" "правильный ответ" "правильна відповідь" "doğru cavab" "Тўғри жавоб"
    CorrectAnswers = l10n lang "Correct answers" "Правильные ответы" "Правильні відповіді" "Doğru cavablar" "Тўғри жавоблар"
}

let regL10n lang = {
    Details = l10n lang "details" "подробности" "подробиці" "Ətraflı" "тафсилотлар"
    Procceed = l10n lang "Login to succeed registration" "Залогиньтесь для продолжения регистрации" "Залогіньтеся для продовження реєстрації" "Qeydiyyata Başla" "Рўйхатдан ўтишни давом эттириш учун ўз логинингизни киритинг"
    Registration = l10n lang "registration" "регистрация" "реєстрація" "qeydiyyat" "Рўйхатдан ўтиш"
    Register = l10n lang "Register" "Зарегистрироваться" "Зареєструватися" "Qeydiyyatdan keçmək" "Рўйхатдан ўтиш"
    RegisteredAs = l10n lang "registered as" "имя команды" "ім'я команди" "komanda adı" "Жамоа номи"
    Status = l10n lang "status" "состояние" "стан" "nəticə" "статус"
    Pending = l10n lang "pending" "ожидание" "очікування" "gözləyin" "кутиш"
    Confirmed = l10n lang "confirmed" "ок" "ок" "uğurlu" "тасдиқлаш"
    Rejected = l10n lang "rejected" "отклонено" "відхилено" "uğrsuz" "қайтариш"
    Edit = l10n lang "Edit" "Изменить" "Змінити" "Redaktə edin" "таҳрирлаш"
    Enter = l10n lang "Enter" "Войти в" "Увійти в" "Redaktə edin" "Кириш"
    LinkForAudience = l10n lang "for audience" "для зрителей" "для глядачів" "izləyicilər üçün" "муҳлисслар"
    TeamName = l10n lang "Team Name" "Имя команды" "Ім'я команди" "Komanda adı" "Жамоанинг номи"
}