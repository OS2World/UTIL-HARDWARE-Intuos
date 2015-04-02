.486p
model flat
ideal

; screen size
xMaxScreen=1280
yMaxScreen=800

extrn DosClose:near
extrn DosCreateThread:near
extrn DosExit:near
extrn DosExitList:near
extrn DosOpen:near
extrn DosSetPriority:near
extrn DosSleep:near
extrn DosSuspendThread:near
extrn DosWrite:near

stack 8192

dataseg
szScreen db '\DEV\XSMOUSE$',0
szTablet db '\DEV\$INTUOS$',0

udataseg
ActionTaken dd ?
BytesDone dd ?
fhScreen dd ?
fhTablet dd ?

udataseg
tidTablet dd ?

codeseg
proc MainRoutine c near
arg @@Mod,@@Nul,@@Env,@@Arg
; determine begin of arguments
  cld ; operate foreward scan
  mov ecx,512 ; max scan length
  mov edi,[@@Arg] ; start address
  repne scasb ; find terminator
; process passed arguments
  mov ebx,offset(PutFingerEvent)
  mov ecx,offset(PutStylusEvent)
  mov esi,offset(PutScreenEvent)
  call ProcessArguments
; open tablet device driver
  call DosOpen c,offset(szTablet),offset(fhTablet),offset(ActionTaken),0,0,1,0192h,0
  test eax,eax ; success
  jnz NotOpenTabletDriver
; open screen device driver
  call DosOpen c,offset(szScreen),offset(fhScreen),offset(ActionTaken),0,0,1,0192h,0
  test eax,eax ; success
  jnz NotOpenScreenDriver
; register termination processing
  call DosExitList c,1,offset(ProcessComplete)
; move cursor to center of screen
  call DosWrite c,[fhScreen],esi,10,offset(BytesDone)
; start tablet input thread
  call DosCreateThread c,offset(tidTablet),offset(TabletThread),0,2,8192
; hang in here forever
  call DosSleep c,-1
label EndProcess near
; force process complete
  sub eax,eax ; success
  ret ; uses exit list
label NotOpenScreenDriver near
; close tablet device driver
  call DosClose c,[fhTablet]
label NotOpenTabletDriver near
; exit the process
  call DosExit c,1,0
endp MainRoutine

codeseg
proc ProcessComplete c near
arg @@ReasonCode
; suspend tablet input thread
  call DosSuspendThread c,[tidTablet]
; close screen device driver
  call DosClose c,[fhScreen]
; close tablet device driver
  call DosClose c,[fhTablet]
; exit termination process
  call DosExitList c,3,0)
endp ProcessComplete

dataseg
SelectTouch db 0

codeseg
proc TabletThread c near
arg @@parameter:dword
; set time critical priority
  call DosSetPriority c,2,3,31,0
; start stylus input process
  cmp [SelectTouch],1 ; touch
  jne StylusProcess ; stylus
  jmp FingerProcess ; finger
label BadTabletThread near
; exit the tablet thread
  call DosExit c,0,0
endp TabletThread

struc tbl
; common tablet buffer
EvntBt0 db 0 ; event flags
EvntBt1 db 0 ; event flags
EvntBt2 db 0 ; event flags
EvntBt3 db 0 ; event flags
xMaxPos dd ? ; tablet width
yMaxPos dd ? ; tablet height
xMaxOut dd xMaxScreen ; width
yMaxOut dd yMaxScreen ; height
xMinOut dd 0 ; horizontal offset
yMinOut dd 0 ; vertical offset
xNewPos dd 0 ; new position
yNewPos dd 0 ; new position
xOldPos dd 0 ; old position
yOldPos dd 0 ; old position
; mouse events left/right
Button1 dw 1806h ; 010/001
Button2 dw 0618h ; 001/010
Button3 dw 6060h ; 100/100
; set maximum positions
xMax0 dw ? ; width
yMax0 dw ? ; height
xMax1 dw ? ; width
yMax1 dw ? ; height
ends tbl

proc MouseActions near
; set mouse button event
  test al,07h ; buttons
  mov ah,001h ; MoveOnly
  jz EndButtons ; none
  mov ah,000h ; NoButMov
; left/right handed mouse
  movzx edx,[ButtonMode]
  add edx,edi ; left/right
; check 1st physical button
  test al,01h ; 1st button
  jz EndButton1 ; nope
; apply 1st logical button
  or ah,[byte(edx+tbl.Button1)]
label EndButton1 near
; check 2nd physical button
  test al,02h ; 2nd button
  jz EndButton2 ; nope
; apply 2nd logical button
  or ah,[byte(edx+tbl.Button2)]
label EndButton2 near
; check 3rd physical button
  test al,04h ; 3rd button
  jz EndButton3 ; nope
; apply 3rd logical button
  or ah,[byte(edx+tbl.Button3)]
label EndButton3 near
label EndButtons near
; return to caller
  ret ; return
endp MouseActions

struc scr
; common event buffer
BtnEvnt dw 1 ; move to center
yCurPos dw yMaxScreen/2 ; center
xCurPos dw xMaxScreen/2 ; center
yMaxPos dw yMaxScreen ; height
xMaxPos dw xMaxScreen ; width
xMaxOut dd xMaxScreen ; width
yMaxOut dd yMaxScreen ; height
xOutPos dd xMaxScreen/2 ; center
yOutPos dd yMaxScreen/2 ; center
; set mouse events left/right
Action0 dw 0101h ; 000/000
Action1 dw 1806h ; 010/001
Action2 dw 0618h ; 001/010
Action3 dw 1E1Eh ; 011/011
Action4 dw 6060h ; 100/100
Action5 dw 7866h ; 101/011
Action6 dw 6678h ; 011/101
Action7 dw 7E7Eh ; 111/111
ends scr

dataseg
CircleMode db 0

codeseg
proc ProperAspect near
; prepare proper active area
  and eax,0Fh ; limit index
  movzx edx,[word(edi+eax*4+tbl.xMax0)]
  mov [edi+tbl.xMaxPos],edx
  movzx edx,[word(edi+eax*4+tbl.yMax0)]
  mov [edi+tbl.yMaxPos],edx
; verify proper aspect ratio
  cmp [CircleMode],1 ; circles
  mov esi,offset(PutScreenEvent)
  jne EndProperAspect ; nope
; fit screen x into tablet
  mov eax,[esi+scr.yMaxOut]
  mul [edi+tbl.xMaxPos]
  div [edi+tbl.yMaxPos]
  cmp eax,[esi+scr.xMaxOut]
  jnb EndxMaxTablet ; fit
  mov eax,[esi+scr.xMaxOut]
label EndxMaxTablet near
  mov [edi+tbl.xMaxOut],eax
  sub eax,[esi+scr.xMaxOut]
  shr eax,1 ; offset position
  mov [edi+tbl.xMinOut],eax
; fit screen y into tablet
  mov eax,[esi+scr.xMaxOut]
  mul [edi+tbl.yMaxPos]
  div [edi+tbl.xMaxPos]
  cmp eax,[esi+scr.yMaxOut]
  jnb EndyMaxTablet ; fit
  mov eax,[esi+scr.yMaxOut]
label EndyMaxTablet near
  mov [edi+tbl.yMaxOut],eax
  sub eax,[esi+scr.yMaxOut]
  shr eax,1 ; offset position
  mov [edi+tbl.yMinOut],eax
label EndProperAspect near
; return to caller
  ret ; return
endp ProperAspect

nMax=8 ; entries

struc flt
; rolling average
FltN dd ? ; filter
UseN dd ? ; entries
PosN dd 0 ; index
SumX dd 0 ; rolling
SumY dd 0 ; rolling
TotX dd 0 ; output
TotY dd 0 ; output
ValX dd nMax dup(0)
ValY dd nMax dup(0)
ends flt

codeseg
proc RollingAverage near
; check reset average
  cmp al,0 ; reset
  jne EndResetAverage
; reset rolling average
  sub eax,eax ; zeroes
  mov [edi+flt.SumX],eax
  mov [edi+flt.SumY],eax
  movzx ebx,[word(esi+10)]
  movzx edx,[word(esi+12)]
  mov ecx,[edi+flt.UseN]
label ResetAverage near
  mov [edi-4+ecx*4+flt.ValX],ebx
  mov [edi-4+ecx*4+flt.ValY],edx
  add [edi+flt.SumX],ebx
  add [edi+flt.SumY],edx
  loop ResetAverage
label EndResetAverage near
; update rolling position
  mov ecx,[edi+flt.PosN]
  dec ecx ; next position
  jns EndFilterIndex ; ok
  add ecx,[edi+flt.UseN]
label EndFilterIndex near
  mov [edi+flt.PosN],ecx
; build current x position
  movzx eax,[word(esi+10)]
; update rolling x amount
  mov ebx,[edi+ecx*4+flt.ValX]
  mov [edi+ecx*4+flt.ValX],eax
  add eax,[edi+flt.SumX]
  sub eax,ebx ; old sample
  mov [edi+flt.SumX],eax
; apply stylus x filter
  mov ebx,[edi+flt.TotX]
  sub ebx,eax ; deviation
  jns xRolPositive ; ok
  neg ebx ; make positive
label xRolPositive near
  cmp ebx,[edi+flt.FltN]
  jnb EndRolSmoothX ; new
  mov eax,[edi+flt.TotX]
label EndRolSmoothX near
  mov [edi+flt.TotX],eax
; new rolling x average
  sub edx,edx ; zeroes
  div [edi+flt.UseN]
; store new x position
  mov [word(esi+10)],ax
; build current y position
  movzx eax,[word(esi+12)]
; update rolling y amount
  mov ebx,[edi+ecx*4+flt.ValY]
  mov [edi+ecx*4+flt.ValY],eax
  add eax,[edi+flt.SumY]
  sub eax,ebx ; old sample
  mov [edi+flt.SumY],eax
; apply stylus y filter
  mov ebx,[edi+flt.TotY]
  sub ebx,eax ; deviation
  jns yRolPositive ; ok
  neg ebx ; make positive
label yRolPositive near
  cmp ebx,[edi+flt.FltN]
  jnb EndRolSmoothY ; new
  mov eax,[edi+flt.TotY]
label EndRolSmoothY near
  mov [edi+flt.TotY],eax
; new rolling y average
  sub edx,edx ; zeroes
  div [edi+flt.UseN]
; store new y position
  mov [word(esi+12)],ax
; return to caller
  ret ; return
endp RollingAverage

dataseg
GetDevDescriptor db 80h,06h,00h,01h,00h,00h,12h,00h,12h dup(0EEh)

codeseg
proc ControlTransfer near
; execute control transfer
  mov [edi+6],al ; data size
  lea edx,[eax+8] ; total size
  call DosWrite c,[fhTablet],edi,edx,offset(BytesDone)
  test eax,eax ; success
; return to caller
  ret ; return
endp ControlTransfer

dataseg
SetConfiguration db 00h,09h,01h,00h,00h,00h,00h,00h
SetFeatureReport db 21h,09h,02h,03h,00h,00h,02h,00h,02h,02h

dataseg
; idProduct<00><01><02><03><04><05><06><07><08><09><0A><0B><0C><0D><0E><0F>
TypeSize db 00h,00h,20h,21h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,20h,00h

codeseg
proc SwitchTablet near
; hang in here some minimum time
  call DosSleep c,50 ; milliseconds
; get device descriptor request
  mov edi,offset(GetDevDescriptor)
  mov eax,12h ; data length
  call ControlTransfer
  jnz SwitchTablet
; set configuration request
  mov edi,offset(SetConfiguration)
  call ControlTransfer
  jnz SwitchTablet
; set feature report request
  mov edi,offset(SetFeatureReport)
  mov al,02h ; data length
  call ControlTransfer
  jnz SwitchTablet
; hang in here some minimum time
  call DosSleep c,50 ; milliseconds
; setup tablet capabilities
  mov al,[GetDevDescriptor+18]
  and eax,0Fh ; limit index
  mov al,[TypeSize+eax]
; return to caller
  ret ; return
endp SwitchTablet

codeseg
proc TabletToScreen c near
arg @@xCurPos,@@yCurPos
; save tablet position
  mov eax,[edi+tbl.xNewPos]
  mov [edi+tbl.xOldPos],eax
  mov eax,[edi+tbl.yNewPos]
  mov [edi+tbl.yOldPos],eax
; convert current xpos
  mov eax,[@@xCurPos]
  mul [edi+tbl.xMaxOut]
  div [edi+tbl.xMaxPos]
  sub eax,[edi+tbl.xMinOut]
  jns xPosNewPosition ; ok
  sub eax,eax ; minimum
label xPosNewPosition near
  mov [edi+tbl.xNewPos],eax
; convert current ypos
  mov eax,[@@yCurPos]
  mul [edi+tbl.yMaxOut]
  div [edi+tbl.yMaxPos]
  sub eax,[edi+tbl.yMinOut]
  jns yPosNewPosition ; ok
  sub eax,eax ; minimum
label yPosNewPosition near
  mov [edi+tbl.yNewPos],eax
; verify screen position
  cmp [edi+tbl.EvntBt2],1
  je UsePosition ; use
  mov [edi+tbl.EvntBt2],1
; save tablet position
  mov eax,[edi+tbl.xNewPos]
  mov [edi+tbl.xOldPos],eax
  mov eax,[edi+tbl.yNewPos]
  mov [edi+tbl.yOldPos],eax
label UsePosition near
; return to caller
  ret ; return
endp TabletToScreen

dataseg
FingerMode dd 1 ; relative
StylusMode dd 0 ; absolute

dataseg
ButtonMode db 0 ; right

dataseg
PutScreenEvent scr {}

dataseg
TabletRotate db 0 ; no

codeseg
proc WriteScreen c near
arg @@MouseMode
local @@BytesDone
; prepare screen work pointer
  mov edx,offset(PutScreenEvent)
; provide current event
  mov al,[edi+tbl.EvntBt0]
  mov [edx],al ; event
; update screen position
  cmp [edi+tbl.EvntBt2],1
  jne WriteEvent ; skip
; obtain tablet position
  mov ebx,[edi+tbl.xNewPos]
  mov ecx,[edi+tbl.yNewPos]
; check relative mode
  mov eax,[@@MouseMode]
  cmp al,1 ; relative
  jne EndMouseMode ; no
; update relative position
  sub ebx,[edi+tbl.xOldPos]
  sub ecx,[edi+tbl.yOldPos]
; update absolute xpos
  add ebx,[edx+scr.xOutPos]
; verify minimum xpos
  jns xPosPositive
  sub ebx,ebx ; minimum
label xPosPositive near
; update absolute ypos
  add ecx,[edx+scr.yOutPos]
; verify minimum ypos
  jns yPosPositive
  sub ecx,ecx ; minimum
label yPosPositive near
label EndMouseMode near
; verify maximum xpos
  cmp ebx,[edx+scr.xMaxOut]
  jna xPosAccepted
  mov ebx,[edx+scr.xMaxOut]
label xPosAccepted near
; verify maximum ypos
  cmp ecx,[edx+scr.yMaxOut]
  jna yPosAccepted
  mov ecx,[edx+scr.yMaxOut]
label yPosAccepted near
; supply screen position
  mov [edx+scr.xOutPos],ebx
  mov [edx+scr.yOutPos],ecx
; rotate tablet position
  cmp [TabletRotate],1
  jne NotRotate ; no
; reverse tablet xpos
  mov eax,[edx+scr.xMaxOut]
  sub eax,ebx ; downwards
  mov [edx+scr.xCurPos],ax
; reverse tablet ypos
  mov eax,[edx+scr.yMaxOut]
  sub eax,ecx ; downwards
  mov [edx+scr.yCurPos],ax
  jmp EndRotate ; done
label NotRotate near
  mov [edx+scr.xCurPos],bx
  mov [edx+scr.yCurPos],cx
label EndRotate near
label WriteEvent near
; write screen event
  lea eax,[@@BytesDone]
  call DosWrite c,[fhScreen],edx,10,eax
; return to caller
  ret ; return
endp WriteScreen

dataseg
GetFingerInput db 0ECh,00h,0FFh,0FFh,83h,03h,14h,00h,14h dup(0EEh)

dataseg
PutFingerEvent tbl {xMax0=480,yMax0=320,xMax1=720,yMax1=480}

udataseg
FingerDone dd ?

dataseg
Finger flt {FltN=1*nMax,UseN=nMax}

codeseg
proc FingerProcess near
; setup finger configuration
  call SwitchTablet ; prepare
; verify finger capabilities
  test al,10h ; finger support
  jz UseStylusEvent ; switch
label UseFingerEvent near
; apply proper aspect ratio
  mov edi,offset(PutFingerEvent)
  call ProperAspect ; finger
  mov esi,offset(GetFingerInput)
; reset finger position
  mov [edi+tbl.EvntBt2],0
label GetFingerEvent near
  mov [byte(esi+6)],14h ; reset
  call DosWrite c,[fhTablet],esi,28,offset(FingerDone)
  test eax,eax ; success
  jnz FingerProcess
; verify finger event
  cmp [byte(esi+8)],02h
  jne GetFingerEvent
; check tablet rotated
  mov al,[esi+9] ; event
  cmp [TabletRotate],1
  jne EndRotation ; ok
; rotate express keys
  sub ecx,ecx ; loop
  mov cl,04h ; counter
label KeyRotate near
  rcr al,1 ; one bit
  rcl ah,1 ; one bit
  loop KeyRotate
  mov al,ah ; event
label EndRotation near
; process finger event
  mov ah,001h ; MoveOnly
  cmp [byte(esi+25)],011h
  je EndPosition ; single
label NotPosition near
; reset finger position
  mov [edi+tbl.EvntBt2],0
  mov ah,000h ; NoButMov
label EndPosition near
  cmp al,000h ; NoButMov
  je FingerToMouseEvent
; obtain mouse actions
  call MouseActions
; process finger events
  cmp al,008h ; Bt0008Down
  jb FingerToMouseEvent
;------------------------------------------------------------------------------
; action switch to stylus
; cmp al,00Ah ; Bt0208Down
; je StylusProcess
;------------------------------------------------------------------------------
; ignore unused buttons
  jmp GetFingerEvent
label FingerToMouseEvent near
; store emulated mouse event
  mov [edi+tbl.EvntBt0],ah
; current finger position
  movzx eax,[byte(esi+12)]
  xor ah,[byte(esi+11)]
  jns WriteFingerEvent
  and ah,01h ; validate xpos
  movzx edx,[byte(esi+14)]
  mov dh,[byte(esi+13)]
  and dh,01h ; validate ypos
; fix current finger position
  mov [esi+10],eax ; xpos
  mov [esi+12],edx ; ypos
; apply rolling average
  mov al,[edi+tbl.EvntBt2]
  mov edi,offset(Finger)
  call RollingAverage
; finger to screen position
  mov edi,offset(PutFingerEvent)
  movzx eax,[word(esi+10)] ; xpos
  movzx edx,[word(esi+12)] ; ypos
  call TabletToScreen c,eax,edx
label WriteFingerEvent near
; write emulated mouse event
  call WriteScreen c,[FingerMode]
  jmp GetFingerEvent
endp FingerProcess

dataseg
GetStylusInput db 0ECh,00h,0FFh,0FFh,81h,03h,0Ah,00h,0Ah dup(0EEh)

dataseg
PutStylusEvent tbl {xMax0=15200,yMax0=9500,xMax1=21600,yMax1=13500}

udataseg
StylusDone dd ?

dataseg
Stylus flt {FltN=8*nMax,UseN=nMax}

codeseg
proc StylusProcess near
; setup stylus configuration
  call SwitchTablet ; prepare
; verify stylus capabilities
  test al,20h ; stylus support
  jz UseFingerEvent ; switch
label UseStylusEvent near
; apply proper aspect ratio
  mov edi,offset(PutStylusEvent)
  call ProperAspect ; stylus
  mov esi,offset(GetStylusInput)
; reset stylus position
  mov [edi+tbl.EvntBt2],0
label GetStylusEvent near
  mov [byte(esi+6)],0Ah ; reset
  call DosWrite c,[fhTablet],esi,18,offset(StylusDone)
  test eax,eax ; success
  jnz StylusProcess
; verify stylus event
  cmp [byte(esi+8)],02h
  jne GetStylusEvent
; process stylus event
  mov al,[esi+9] ; event
; accept border events
  and al,0EFh ; +border
; reset stylus events
  cmp al,0E0h ; Et0000Down
  jb ResetStylusEvent
; obtain mouse actions
  call MouseActions
; process stylus events
  cmp al,0E8h ; Et0008Down
  jb StylusToMouseEvent
;------------------------------------------------------------------------------
; action switch to finger
; cmp al,0E9h ; Et1008Down
; cmp al,0EBh ; Et1208Down
; cmp al,0EDh ; Et1048Down
; je FingerProcess
;------------------------------------------------------------------------------
; ignore unused buttons
  jmp GetStylusEvent
label ResetStylusEvent near
; reset stylus position
  mov [edi+tbl.EvntBt2],0
; ensure buttons released
  mov ah,000h ; NoButMov
  cmp [edi+tbl.EvntBt0],ah
  je GetStylusEvent ; ok
  mov [edi+tbl.EvntBt0],ah
  jmp WriteStylusEvent
label StylusToMouseEvent near
; store emulated mouse event
  mov [edi+tbl.EvntBt0],ah
; apply rolling average
  mov al,[edi+tbl.EvntBt2]
  mov edi,offset(Stylus)
  call RollingAverage
; stylus to screen position
  mov edi,offset(PutStylusEvent)
  movzx eax,[word(esi+10)] ; xpos
  movzx edx,[word(esi+12)] ; ypos
  call TabletToScreen c,eax,edx
label WriteStylusEvent near
; write emulated mouse event
  call WriteScreen c,[StylusMode]
  jmp GetStylusEvent
endp StylusProcess

codeseg
proc dec2bin near
; decimal to binary
  sub eax,eax ; input
  sub edx,edx ; output
label ConvertInput near
  inc edi ; next position
  mov al,[edi] ; digit
; convert decimal digit
  cmp al,'0' ; minimum
  jb Enddec2bin ; done
  cmp al,'9' ; maximum
  ja Enddec2bin ; done
  sub al,'0' ; digit
  lea edx,[edx*4+edx]
  lea edx,[edx*2+eax]
  jmp ConvertInput
label Enddec2bin Near
; return to caller
  ret ; return
endp dec2bin

codeseg
proc MouseButtons c near
uses ecx ; button index
; initialize button index
  sub ecx,ecx ; 1st button
label UpdateButton near
; verify action index
  inc edi ; next position
  movzx eax,[byte(edi)]
  cmp al,'0' ; minimum
  jb EndMouseButtons
  cmp al,'7' ; maximum
  ja EndMouseButtons
; update logical button
  and al,07h ; action index
  mov ax,[esi+eax*2+scr.Action0]
  mov [edx+ecx*2+tbl.Button1],ax
  inc ecx ; next button index
  cmp cl,03h ; last button
  jb UpdateButton ; next
label EndMouseButtons near
; return to caller
  ret ; return
endp MouseButtons

codeseg
proc ProcessArguments near
; scan for forward slash
  mov al,[edi] ; character
  inc edi ; next position
  cmp al,00h ; terminator
  je EndScanString ; done
  cmp al,'/' ; parameter
  jne ProcessArguments
; allow upper/lower case
  mov al,[edi] ; character
  or al,20h; ignore case
; finger absolute mode
  cmp al,'a' ; absolute
  jne NotAbsoluteMode
  mov [byte(FingerMode)],0
  jmp ProcessArguments
label NotAbsoluteMode near
; stylus button setting
  cmp al,'b' ; buttons
  jne NotStylusButton
  mov edx,ecx ; stylus
  call MouseButtons
  jmp ProcessArguments
label NotStylusButton near
; stylus correct circles
  cmp al,'c' ; circles
  jne NotCircleMode
  mov [CircleMode],1
  jmp ProcessArguments
label NotCircleMode near
; finger button setting
  cmp al,'k' ; keys
  jne NotFingerButton
  mov edx,ebx ; finger
  call MouseButtons
  jmp ProcessArguments
label NotFingerButton near
; left handed mouse set
  cmp al,'l' ; lefty
  jne NotLeftHanded
  mov [ButtonMode],1
  jmp ProcessArguments
label NotLeftHanded near
; stylus relative mode
  cmp al,'m' ; mouse
  jne NotRelativeMode
  mov [byte(StylusMode)],1
label ProceszArguments near
; intermediate jump back
  jmp ProcessArguments
label NotRelativeMode near
; tablet rotate setting
  cmp al,'r' ; rotate
  jne NotTabletRotate
  mov [TabletRotate],1
  jmp ProceszArguments
label NotTabletRotate near
; initial touch setting
  cmp al,'t' ; touch
  jne NotSelectTouch
  mov [SelectTouch],1
  jmp ProceszArguments
label NotSelectTouch near
; screen height setting
  cmp al,'h' ; height
  jne NotScreenHeight
  call dec2bin ; convert
; verify screen height
  cmp edx,1024*32 ; max
  ja ProceszArguments
  cmp edx,1024/32 ; min
  jb ProceszArguments
; update tablet heights
  mov [ebx+tbl.yMaxOut],edx
  mov [ecx+tbl.yMaxOut],edx
; update screen heights
  mov [esi+scr.yMaxPos],dx
  mov [esi+scr.yMaxOut],edx
  shr edx,1 ; center position
  mov [esi+scr.yOutPos],edx
  mov [esi+scr.yCurPos],dx
  jmp ProceszArguments
label NotScreenHeight near
; screen width setting
  cmp al,'w' ; width
  jne ProceszArguments
  call dec2bin ; convert
; verify screen width
  cmp edx,1024*32 ; max
  ja ProceszArguments
  cmp edx,1024/32 ; min
  jb ProceszArguments
; update tablet widths
  mov [ebx+tbl.xMaxOut],edx
  mov [ecx+tbl.xMaxOut],edx
; update screen widths
  mov [esi+scr.xMaxPos],dx
  mov [esi+scr.xMaxOut],edx
  shr edx,1 ; center position
  mov [esi+scr.xOutPos],edx
  mov [esi+scr.xCurPos],dx
  jmp ProceszArguments
label EndScanString near
; return to caller
  ret ; return
endp ProcessArguments

end MainRoutine
