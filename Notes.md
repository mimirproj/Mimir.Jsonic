dotnet nuget locals all --clear

dotnet pack ./src/Core/Mimir.Jsonic.fsproj -c Release
dotnet pack ./src/Fable/Mimir.Jsonic.Fable.fsproj -c Release
dotnet pack ./src/Net/Mimir.Jsonic.Net.fsproj -c Release
dotnet pack ./src/Saturn/Mimir.Jsonic.Saturn.fsproj -c Release