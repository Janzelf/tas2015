; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
; NB op TWEE plaatsen veranderen!!
#define Versie "2019.1A"

[Languages]
Name: "nl"; MessagesFile: "Dutch.isl"

[Setup]
AppName=ToernooiAssistent
AppVerName=ToernooiAssistent update
AppVersion={#Versie}
AppPublisher=ToernooiKlapper.nl
DefaultDirName={param:MYPATH|}
DisableStartupPrompt=true
Uninstallable=yes
DirExistsWarning=false
DisableDirPage=true
;DisableFinishedPage=true
DisableReadyMemo=yes
;OutputBaseFilename=TAupdate
UsePreviousAppDir=no
OutputBaseFilename=TA20191A
;InfoAfterFile=totslot.rtf
OutputDir="C:\proxy"
;AlwaysRestart=yes

[Code]

function InitializeSetup(): Boolean;
var
	sPrevPath: String;
begin
  sPrevPath := expandConstant('{param:MYPATH|}');
  if ( Length(sPrevPath) = 0 ) then begin
     MsgBox( 'Geen installatiepad opgegeven.' + #13 + #10
         + 'Kan niet updaten!' ,  mbCriticalError, MB_OK );
      Result := false;
      end
  else Result := true;
end;


[Files]
Source: "C:\Users\Jan de Lint\Dropbox\tas2015\EXE\TASVP1.exe"; DestDir: "{app}"; Flags: ignoreversion
;Source: "..\..\maaklicentie\exe\Demo1\TASVP1.exe"; DestDir: "{app}"; Flags: ignoreversion
;Source: "WBCtrl.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "libexpat.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "tasinet.chm"; DestDir: "{app}"; Flags: ignoreversion
Source: "PROLOG.ERR"; DestDir: "{app}"; Flags: ignoreversion
Source: "VLAGGEN\*"; DestDir: "{app}\Vlaggen"; Flags: ignoreversion
;Source: "maakform4.js"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\maakform.css"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\libcurl.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\zlibwapi.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\maakformklapper.html"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\maakform4.html"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\Demo-oefentoernooi.tas"; DestDir: "{userdocs}\Toernooien"; Flags: ignoreversion; 


[Registry]
Root: HKCR; Subkey: ".tas"; ValueType: string; ValueName: ""; ValueData: "Toernooibestand"; Flags: uninsdeletevalue 
Root: HKCR; Subkey: "Toernooibestand"; ValueType: string; ValueName: ""; ValueData: "Toernooibestand"; Flags: uninsdeletekey 
Root: HKCR; Subkey: "Toernooibestand\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\tasvp1.exe,0" 
Root: HKCR; Subkey: "Toernooibestand\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\tasvp1.exe"" ""%1""" 






