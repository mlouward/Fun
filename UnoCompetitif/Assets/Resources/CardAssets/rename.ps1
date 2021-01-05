$files = Get-ChildItem "." -Filter *.png
# foreach ($item in $files) {
#     Write-Output $item.Name
#     Rename-Item -Path $item.Name "Card_$($item.Name.Split('.')[0])_.png"
# }
for ($i = 30; $i -lt 40; $i++) {
    # Rename-Item Card_$($i)_.png Card_$($i)_1.png 
    $name = $i - 30
    Rename-Item Card_$($i)_.png Card_$($name)_3.png 
}