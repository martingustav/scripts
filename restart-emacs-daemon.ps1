# restart-emacs-daemon.ps1
Get-Process emacs -ErrorAction SilentlyContinue | Stop-Process -Force
Start-Sleep -Seconds 1
& "C:\Program Files\Emacs\emacs-30.2\bin\runemacs.exe" --daemon