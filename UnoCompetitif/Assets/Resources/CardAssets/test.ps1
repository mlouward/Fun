$path = "."
$cards = Import-Csv "$($path)\UNO_CSV.csv"
$i = 0
$ProgressPreference = 'SilentlyContinue'
foreach ($item in $cards) {
    $link = $item.Image
    Invoke-WebRequest -Uri $link -OutFile "$($i).png"
    Write-Host $i / $cards.length
    $i++
}
$ProgressPreference = 'Continue'