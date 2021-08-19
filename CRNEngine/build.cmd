@echo off
cls

dotnet tool restore

dotnet restore dotnet-fake.proj
dotnet fake build target %*