FROM mcr.microsoft.com/dotnet/core/aspnet:3.1-alpine
COPY /deploy /
WORKDIR /
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Server.dll" ]
