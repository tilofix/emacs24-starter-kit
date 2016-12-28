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

$RootInstallDirectory = "C:\cygwin64"
$LocalDirectory = "D:\zip\cygwin-x86_64"
$SetupExe = "setup-x86_64.exe"
$PathToSetupExe = Join-Path -Path $LocalDirectory -ChildPath $SetupExe 
$Site1Url = "http://gd.tuwien.ac.at/gnu/cygwin/"
$Site2Url = "http://gd.tuwien.ac.at/gnu/sourceware/cygwinports/"
$Site2Key = "http://cygwinports.org/ports.gpg"
$PackagesTxt = "cygwin_packages.pkg"
$PathToPackagesTxt = Join-Path -Path $LocalDirectory -ChildPath $PackagesTxt 
$InstallPackages = Get-Content -Path $PathToPackagesTxt | ForEach-Object {Write-Output "$_ "}
$SetupExeArgumentList = "--upgrade-also --quiet-mode --local-package-dir $LocalDirectory --root $RootInstallDirectory --site $Site1Url --site $Site2Url --pubkey $Site2Key --packages $InstallPackages"

Write-Host "ArgumentList used for Setup.exe: $SetupExeArgumentList"

# Run your code that needs to be elevated here
(new-object System.Net.WebClient).DownloadFile('http://cygwin.com/setup-x86_64.exe', $PathToSetupExe)

if (!$?) {
   Write-Host "Something wrong happened when downloading the Cygwin installer."
   Write-Host -NoNewLine "Press any key to continue..."
   $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
   exit
}

# When you use the PassThru parameter, Start-Process generates a System.Diagnostics.Process. 
$p = Start-Process $PathToSetupExe -ArgumentList $SetupExeArgumentList -wait -NoNewWindow -PassThru 

if ($p.ExitCode -ne 0) {
   Write-Host "Cygwin setup failed with an error!"
}

Remove-Item $PathToSetupExe

Write-Host -NoNewLine "Press any key to continue..."
$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
