# Open Quiz Platform

Open-Quiz is a web platform for producing online quizzes. Regulations of What Where When and Trivia Quizzes are supported. Audio/video stream may be embeded into a quiz. Media content is allowed (images, audio and video). There are no limitations on the number of participants!

[www.open-quiz.com](https://www.open-quiz.com)

The service is completely free for commercial and noncommercial use.

Five steps to produce your quiz:

* [Login](https://www.open-quiz.com/login)
* [Setup Quiz](https://github.com/usix79/openquiz/blob/master/src/Client/public/manual.html#p2)
* [Prepare Questions Package](https://github.com/usix79/openquiz/blob/master/src/Client/public/manual.html#p3)
* [Publish Registration Link](https://github.com/usix79/openquiz/blob/master/src/Client/public/manual.html#p5.1)
* [Go Live!](https://github.com/usix79/openquiz/blob/master/src/Client/public/manual.html#p5.2)

See [Release Notes](https://github.com/usix79/openquiz/blob/master/RELEASE_NOTES.md) for information of latest updates.

# Development Prerequisites

 * [.NET SDK 5.0](https://dotnet.microsoft.com/download/dotnet-core/)
 * [node.js](https://nodejs.org/)
 * [npm](https://www.npmjs.com/)
 * [AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/install-cliv2.html)

Configure aws defaults:
```
aws configure
```

Install dotnet tools:
```
dotnet tool restore
```

# Development Targets

| Target | Command |
| ------ | ------- |
| Prepare development infrastructure | `dotnet fake build -t devenv` |
| Run locally | `dotnet fake build -t run` |
| Deploy | `dotnet fake build -t deploy` |
