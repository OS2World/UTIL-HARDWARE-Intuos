intuos.exe: intuos.def intuos.obj
  link386 /a:16 /map /nod intuos,intuos.exe,,os2,intuos

intuos.obj: intuos.asm intuos.mak
  tasm /la /m /oi intuos.asm,intuos.obj
