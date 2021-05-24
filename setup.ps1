# Noc setup for Windows systems

function Installation($local) {
  echo "Copying Noc library ..."
  stack install
	
  try {
    echo "Copying Noc library ..."
    New-Item -Path $local -Name "\noc\std" -ItemType "directory" -ErrorAction Stop
    Copy-Item -Path "std\*" -Destination "$($local)\noc\std" -Recurse 
    Write-Host "Noc installation is done." -ForegroundColor Green
  } catch { Write-Host "ERROR! The '$($local)\noc' directory already exists." -ForegroundColor Red }
}

function Uninstallation($local) {
   try {
     echo "Removing Noc binary ..."
     Remove-Item "$($local)\bin\noc.exe" -ErrorAction Stop
     
     try {
       echo "Remove Noc library ..."
       Remove-Item "$($local)\noc" -Recurse -ErrorAction Stop
       Write-Host "Noc uninstallation is done." -ForegroundColor Green
     } catch { Write-Host "ERROR! The '$($local)\noc' directory not found." -ForegroundColor Red }

   } catch { Write-Host "ERROR! The '$($local)\bin\noc.exe' file not found." -ForegroundColor Red  } 
}

# ---------------------------

$arg=$args[0]
$LOCAL_PATH="$($env:APPDATA)\local"

If($arg -eq "install") 
{ 
  Installation $LOCAL_PATH
} 
elseif($arg -eq "uninstall") 
{
  Uninstallation $LOCAL_PATH
} 
else 
{
  Write-Host "Wrong argument or no positional argument." -ForegroundColor Red
}
