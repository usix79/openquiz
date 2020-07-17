module L10n

type Languages =
    | English
    | Russian
    | Ukrainian

type CommonL10n = {
    Question : string
    Answer : string
    Comment : string
    PointsSmall : string
    Points : string
    Place : string
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
    TeamName : string
}

let private l10n lang en ru ua =
    match lang with
    | English -> en
    | Russian -> ru
    | Ukrainian -> ua

let commonL10n lang = {
    Question = l10n lang "Question" "Вопрос" "Питання"
    Answer = l10n lang "Answer" "Ответ" "Відповідь"
    Comment = l10n lang "Comment" "Комментарий" "Коментар"
    PointsSmall = l10n lang "points" "очка" "очки"
    Points = l10n lang "Points" "Очки" "Очки"
    Place = l10n lang "Place" "Место" "Місце"
    Team = l10n lang "Team" "Команда" "Команда"
    QuizIsNotStarted = l10n lang "Quiz is not started" "Квиз еще не начался" "Квіз ще не почався"
    QuizIsNotFinished = l10n lang "Quiz is finished" "Квиз завершен" "Квіз завершено"
    MenuQuestion = l10n lang "Question" "Вопрос" "Питання"
    MenuHistory = l10n lang "History" "История" "Історія"
    MenuResults = l10n lang "Results" "Итоги" "Підсумки"
    MenuLastCall = l10n lang "Last Call" "Время" "Час"
}

let teamL10n lang = {
    Common = commonL10n lang
    NotActive = l10n lang "NOT ACTIVE" "НЕАКТИВНО" "НЕ АКТИВНО"
    AnotherSession = l10n lang "Team's session is running from another device. Only one device is allowed per team." "Капитанская сессия запущена на другом устройстве. Допускается лишь одна активная сессия в команде." "Капітанська сесія запущена на іншому пристрої. Допускається лише одна активна сесія в команді."
    UseThisDevice = l10n lang "Activate" "Активировать" "Активувати"
    YourAnswer = l10n lang "Your answer" "Ваш ответ" "Ваша відповідь"
    WaitingForConfirmation = l10n lang "Waiting for confirmation of the registration..." "Ожидается подтверждение регистрации..." "Очікується підтвердження реєстрації..."
    RegistrationHasBeenRejected = l10n lang "Registration has been rejected (" "Регистрация была отклонена (" "Реєстрація була відхилена ("
    Jeopardy = l10n lang "Jeopardy!" "Рискую!" "Ризикую!"
    Question = l10n lang "Question" "Вопрос" "Питання"
    Answer = l10n lang "Answer" "Ответ" "Відповідь"
    Points = l10n lang "Points" "Очки" "Очки"
    CorrectAnswer = l10n lang "correct answer" "правильный ответ" "правильну відповідь"
}

let audienceL10n lang = {
    Common = commonL10n lang
    Audience = l10n lang "audience" "зрители" "глядачі"
    Question = l10n lang "Question" "Вопрос" "Питання"
    CorrectAnswer = l10n lang "correct answer" "правильный ответ" "правильну відповідь"
    CorrectAnswers = l10n lang "Correct answers" "Правильные ответы" "Правильні відповіді"
}

let regL10n lang = {
    Details = l10n lang "details" "подробности" "подробиці"
    Procceed = l10n lang "Login to succeed registration" "Залогиньтесь для продолжения регистрации" "Залогіньтеся для продовження реєстрації"
    Registration = l10n lang "registration" "регистрация" "реєстрація"
    Register = l10n lang "Register" "Зарегистрироваться" "Зареєструватися"
    RegisteredAs = l10n lang "registered as" "имя команды" "ім'я команди"
    Status = l10n lang "status" "состояние" "стан"
    Pending = l10n lang "pending" "ожидание" "очікування"
    Confirmed = l10n lang "confirmed" "ок" "ок"
    Rejected = l10n lang "rejected" "отклонено" "відхилено"
    Edit = l10n lang "Edit" "Изменить" "Змінити"
    Enter = l10n lang "Enter" "Войти в" "Увійти в"
    TeamName = l10n lang "Team Name" "Имя команды" "Ім'я команди"
}