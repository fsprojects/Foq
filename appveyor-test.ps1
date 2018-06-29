# run tests
dotnet msbuild /m Tests/tests.proj

# upload results to AppVeyor
$testResults = Get-ChildItem -Recurse -Filter *.trx
$wc = New-Object 'System.Net.WebClient'
$testResults | ForEach-Object { $wc.UploadFile("https://ci.appveyor.com/api/testresults/mstest/$($env:APPVEYOR_JOB_ID)", $_.FullName)}