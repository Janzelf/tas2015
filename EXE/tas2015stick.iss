; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#define Versie "2019.1A"

[Languages]
Name: "nl"; MessagesFile: "Dutch.isl"

[Setup]
AppName=ToernooiAssistent
AppVerName=ToernooiAssistent update USB
AppVersion={#Versie}
AppPublisher=ToernooiKlapper.nl
DefaultDirName={code:TasDir}
DisableStartupPrompt=true
Uninstallable=no
DirExistsWarning=false
DisableDirPage=true
;DisableFinishedPage=true
DisableReadyMemo=yes
;OutputBaseFilename=TAupdate
UsePreviousAppDir=no
OutputBaseFilename=TA20191AUSB
OutputDir="C:\proxy"
InfoAfterFile=totslotusb.rtf
;OutputDir="F:\"
;OutputDir="Z:\Program Files\Apache Group\Apache2\proxy"
;OutputDir="Z:\Program Files\Apache Group\Apache2\webpages\distrdam"
;OutputDir = "C:\Kopie van Toernoo3 a\novk2008"
;AlwaysRestart=yes
PrivilegesRequired=none

[Code]

function GetPathInstalled( ): String;
var
   sPrevPath: String;
begin
  sPrevPath := expandConstant('{param:MYPATH|}');

 { MsgBox('Installatiepad' + #13 + #10 +
      sPrevPath
      ,mbInformation, MB_OK );
  }
  
  Result := sPrevPath;
end;


function InitializeSetup(): Boolean;
var
  sInhoud: AnsiString;
  iPos: Integer;
begin
    Result := true;
    if not FileExists(GetPathInstalled()+ '\tasi.nst') then begin
     MsgBox('Kan niet updaten' + #13 + #10 +
      GetPathInstalled()+ '\tasi.nst'  + #13 + #10 +
      'installatie op de USB-stick moet worden herzien!'
      ,mbInformation, MB_OK );
      Result := false;
    end else begin
      LoadStringFromFile(GetPathInstalled()+ '\tasi.nst', sInhoud);
      iPos := Pos('usb(', sInhoud);
      if iPos = 0 then
        SaveStringToFile(GetPathInstalled()+ '\tasi.nst', 'usb(ja)', True);
    end;
end;


function TasDir(Default: String): String;
begin
  Result := GetPathInstalled();
end;

[Files]
Source: "C:\Users\Jan de Lint\Dropbox\tas2015\EXE\TASVP1.exe"; DestDir: "{app}"; Flags: ignoreversion
;Source: "..\..\maaklicentie\exe\Demo1\TASVP1.exe"; DestDir: "{app}"; Flags: ignoreversion
;Source: "WBCtrl.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "libexpat.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "tasinet.chm"; DestDir: "{app}"; Flags: ignoreversion
Source: "PROLOG.ERR"; DestDir: "{app}"; Flags: ignoreversion
Source: "VLAGGEN\*"; DestDir: "{app}\Vlaggen"; Flags: ignoreversion
;Source: "C:\TASSHIP\DEMO1\maakform4.js"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\maakform.css"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\libcurl.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\zlibwapi.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\maakformklapper.html"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\maaklicentie\Exe\Demo1\maakform4.html"; DestDir: "{app}"; Flags: ignoreversion
;Source: "C:\TASSHIP\DEMO1\tasn.dat"; DestDir: "{commonappdata}\Toernooien\demo"; Flags: ignoreversion ; Check: IsVista
;Source: "C:\TASSHIP\DEMO1\tasn.nst"; DestDir: "{commonappdata}\Toernooien\demo"; Flags: ignoreversion restartreplace; Check: IsVista
;Source: "{app}\demo\tasn.dat"; DestDir: "{app}\demo\backup"; Flags: external skipifsourcedoesntexist uninsneveruninstall
;Source: "{app}\demo\tasn.log"; DestDir: "{app}\demo\backup"; Flags: external skipifsourcedoesntexist uninsneveruninstall
;Source: "{app}\demo\tasn.nst"; DestDir: "{app}\demo\backup"; Flags: external skipifsourcedoesntexist uninsneveruninstall
;Source: "C:\TASSHIP\DEMO1\tasn.dat"; DestDir: "{app}\demo"; Flags: ignoreversion;
Source: "..\..\maaklicentie\Exe\Demo1\Demo-oefentoernooi.tas"; DestDir: "{app}\Toernooien"; Flags: ignoreversion; 
;Source: "C:\Tasship\Sleutel\tasi.nst"; DestDir: "{app}"; Flags: ignoreversion
;Source: "{app}\tasi.nst"; DestDir: "{commonappdata}\Toernooien\demo"; DestName: tasn.nst; Flags: external skipifsourcedoesntexist; Check: IsVista
;Source: "C:\TAS2008\EXE\TASVP1.HLP"; DestDir: "{commonappdata}\Toernooien"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files








