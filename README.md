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

## Development Prerequisites

* [.NET SDK 8.0](https://dotnet.microsoft.com/en-us/download/dotnet/8.0)
* [node.js and npm](https://nodejs.org/)
* [AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/install-cliv2.html)

Configure aws defaults:

``` bash
aws configure
```

## Development Targets

| Target | Command |
| ------ | ------- |
| Restore environment | `dotnet fsi make.fsx restore` |
| Prepare development infrastructure | `dotnet fsi make.fsx devenv` |
| Run locally | `dotnet fsi make.fsx run` |
| Deploy | `dotnet fsi make.fsx deploy` |

## AWS Setup

You must manually update CloudFront Distribution to add ApiOrigin
