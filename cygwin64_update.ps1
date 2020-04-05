# Script-Hint: http://superuser.com/questions/214831/how-to-update-cygwin-from-cygwins-command-line/1073082#1073082
# Script-Origin: http://pastebin.com/wMRctAuL

# Get the ID and security principal of the current user account
$myWindowsID=[System.Security.Principal.WindowsIdentity]::GetCurrent()
$myWindowsPrincipal=new-object System.Security.Principal.WindowsPrincipal($myWindowsID)

# Get the security principal for the Administrator role
$adminRole=[System.Security.Principal.WindowsBuiltInRole]::Administrator

# Check to see if we are currently running "as Administrator"
if ($myWindowsPrincipal.IsInRole($adminRole)) {
   # We are running "as Administrator" - so change the title and background color to indicate this
   $Host.UI.RawUI.WindowTitle = $myInvocation.MyCommand.Definition + "(Elevated)"
   $Host.UI.RawUI.BackgroundColor = "DarkBlue"
   clear-host
}
else {
   # We are not running "as Administrator" - so relaunch as administrator
   
   # Create a new process object that starts PowerShell
   $newProcess = new-object System.Diagnostics.ProcessStartInfo "PowerShell";
   
   # Specify the current script path and name as a parameter
   $newProcess.Arguments = $myInvocation.MyCommand.Definition;
   
   # Indicate that the process should be elevated
   $newProcess.Verb = "runas";
   
   # Start the new process
   [System.Diagnostics.Process]::Start($newProcess);
   
   # Exit from the current, unelevated, process
   exit
}

# Run your code that needs to be elevated here

# http://stackoverflow.com/questions/1183183/path-of-currently-executing-powershell-script#1183197
$PathToThisScript = Split-Path $SCRIPT:MyInvocation.MyCommand.Path -parent

$SettingsXml = "cygwin64_settings_$($ENV:computername).xml"
$PathToSettingsXml = Join-Path -Path $PathToThisScript -ChildPath $SettingsXml
# http://exchangeserverpro.com/using-xml-settings-file-powershell-scripts/
[xml]$SettingsFile = Get-Content $PathToSettingsXml 

$RootInstallDirectory = $SettingsFile.Settings.Setup.RootInstallDir
$LocalDirectory = $SettingsFile.Settings.Setup.LocalDir.Trim()
$SetupExe = $SettingsFile.Settings.Setup.SetupExe.Trim()
$PathToSetupExe = Join-Path -Path $LocalDirectory -ChildPath $SetupExe 
$CygwinSiteUrl = $SettingsFile.Settings.Setup.CygwinSiteUrl.Trim()

# CygwinPorts has been closed down: 
# Post in mail list "cygwin-ports-general"
# Project Update:  Yaakov Selkowitz Tue, 21 Mar 2017 02:26:13 -0700 
# "... the external Cygwin Ports repository previously hosted on cygwinports.org 
# has been removed, with the focus of the project 
# shifting solely on maintaining the Cygwin distribution." 

$PackagesTxt = $SettingsFile.Settings.PackageFiles
$PathsToPackagesTxt = $PackagesTxt.ChildNodes | ForEach-Object {if($_.Name -match 'PackageFile') {Join-Path -Path $PathToThisScript -ChildPath $_.InnerText}}
$InstallPackages = ForEach-Object {Get-Content -Path $_} -InputObject $PathsToPackagesTxt | ForEach-Object {if($_ -match '^[^#]') {Write-Output "--packages $_"}}

# Original list of arguments
# $SetupExeArgumentList = "--upgrade-also --quiet-mode --no-desktop --local-package-dir $LocalDirectory --root $RootInstallDirectory --site $CygwinSiteUrl $InstallPackages"
# List of arguments when encoutering issues like "download error in unattended_mode"
# " --package-manager" displays only the package manager at the beginning, issue not solved
$SetupExeArgumentList = "--upgrade-also --no-desktop --local-package-dir $LocalDirectory --root $RootInstallDirectory --site $CygwinSiteUrl $InstallPackages"
# Following packages had download errors:
#   socat-2.0.0-b8-1 --> package search displays 'socat-2.0.0-b9-1'

# http://stackoverflow.com/questions/16906170/create-directory-if-it-does-not-exist#16911470
$isLocalDirectory = Test-Path -PathType Container $LocalDirectory
if (! $isLocalDirectory ) {
    New-Item -ItemType Directory -Path $LocalDirectory
}

Write-Host "ArgumentList used for Setup.exe: $SetupExeArgumentList"

function Download-CygwinSetupExe {
    (new-object System.Net.WebClient).DownloadFile("http://cygwin.com/$SetupExe", $PathToSetupExe)
    if (!$?) {
        Write-Host "Something wrong happened when downloading the Cygwin installer."
        Write-Host -NoNewLine "Press any key to continue..."
        $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
        exit
    }
}

$isPathToSetupExe = Test-Path -PathType Leaf $PathToSetupExe
if (! $isPathToSetupExe ) {
    Write-Host "Download CygwinSetupExe into file: $PathToSetupExe"
    Download-CygwinSetupExe($PathToSetupExe)
} else {
    Write-Host "Local CygwinSetupExe in file: $PathToSetupExe"
}

# When you use the PassThru parameter, Start-Process generates a System.Diagnostics.Process. 
$p = Start-Process $PathToSetupExe -ArgumentList $SetupExeArgumentList -wait -NoNewWindow -PassThru 

if ($p.ExitCode -ne 0) {
   Write-Host "Cygwin setup failed with an error!"
}

# Remove-Item $PathToSetupExe

Write-Host -NoNewLine "Press any key to continue..."
$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
