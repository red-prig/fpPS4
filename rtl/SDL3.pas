unit SDL3;

{$mode ObjFPC}{$H+}

interface

const
  SDL_LibName = 'SDL3.dll';

const
  SDL_INIT_AUDIO    = $00000010; { `SDL_INIT_AUDIO` implies `SDL_INIT_EVENTS`  }
  SDL_INIT_VIDEO    = $00000020; { `SDL_INIT_VIDEO` implies `SDL_INIT_EVENTS`, should be initialized on the main thread  }
  SDL_INIT_JOYSTICK = $00000200; { `SDL_INIT_JOYSTICK` implies `SDL_INIT_EVENTS`  }
  SDL_INIT_HAPTIC   = $00001000;
  SDL_INIT_GAMEPAD  = $00002000; { `SDL_INIT_GAMEPAD` implies `SDL_INIT_JOYSTICK`  }
  SDL_INIT_EVENTS   = $00004000;
  SDL_INIT_SENSOR   = $00008000; { `SDL_INIT_SENSOR` implies `SDL_INIT_EVENTS`  }
  SDL_INIT_CAMERA   = $00010000; { `SDL_INIT_CAMERA` implies `SDL_INIT_EVENTS`  }

const
  SDL_APP_CONTINUE = 0; {*< Value that requests that the app continue from the main callbacks.  }
  SDL_APP_SUCCESS  = 1; {*< Value that requests termination with success from the main callbacks.  }
  SDL_APP_FAILURE  = 2; {*< Value that requests termination with error from the main callbacks.  }

const
  SDL_HINT_APP_NAME     = 'SDL_APP_NAME';
  SDL_HINT_AUDIO_DRIVER = 'SDL_AUDIO_DRIVER';

const
  SDL_EVENT_USER = ($8000);

type
  PPSDL_Event = ^PSDL_Event;
  PSDL_Event = ^TSDL_Event;
  TSDL_Event = record
      case Integer of
         0: (type_: Integer);
           //....
        37: (padding: array[0..127] of Byte);
  end;

function  SDL_WasInit      (flags: Integer): Integer;
function  SDL_InitSubSystem(flags: Integer): Boolean;
procedure SDL_QuitSubSystem(flags: Integer);
procedure SDL_free         (mem: Pointer);
function  SDL_PollEvent    (event: PSDL_Event): Boolean;
function  SDL_WaitEvent    (event: PSDL_Event): Boolean;

var
 sdl3_lib_handle:TLibHandle=NilHandle;

implementation

var
 //
 //function SDL_SetHint(name: PAnsiChar; value: PAnsiChar): Boolean; cdecl;
 //function SDL_SetHintWithPriority(name: PAnsiChar; value: PAnsiChar; priority: TSDL_HintPriority): Boolean; cdecl;
 _SDL_Init         :function (flags: Integer): Boolean; cdecl;
 _SDL_InitSubSystem:function (flags: Integer): Boolean; cdecl;
 _SDL_QuitSubSystem:procedure(flags: Integer); cdecl;
 _SDL_WasInit      :function (flags: Integer): Integer; cdecl;
 _SDL_free         :procedure(mem: Pointer); cdecl;
 _SDL_PollEvent    :function (event: PSDL_Event): Boolean; cdecl;
 _SDL_WaitEvent    :function (event: PSDL_Event): Boolean; cdecl;
 //

function SDL_WasInit(flags: Integer): Integer;
begin
 Result:=0;
 if (_SDL_WasInit<>nil) then
 begin
  Result:=_SDL_WasInit(flags);
 end;
end;

procedure SDL_free(mem: Pointer);
begin
 if (_SDL_free<>nil) then
 begin
  _SDL_free(mem);
 end;
end;

function SDL_PollEvent(event: PSDL_Event): Boolean;
begin
 Result:=False;
 if (_SDL_PollEvent<>nil) then
 begin
  Result:=_SDL_PollEvent(event);
 end;
end;

function SDL_WaitEvent(event: PSDL_Event): Boolean;
begin
 Result:=False;
 if (_SDL_WaitEvent<>nil) then
 begin
  Result:=_SDL_WaitEvent(event);
 end;
end;

function SDL_InitSubSystem(flags: Integer): Boolean;
var
 first_init:Boolean;
begin
 first_init:=False;
 if (sdl3_lib_handle=NilHandle) then
 begin
  sdl3_lib_handle:=SafeLoadLibrary(SDL_LibName);
  if (sdl3_lib_handle=NilHandle) then Exit(False);
  first_init:=True;
 end;

 if first_init then
 begin
  Pointer(_SDL_Init)         :=GetProcedureAddress(sdl3_lib_handle,'SDL_Init');
  Pointer(_SDL_InitSubSystem):=GetProcedureAddress(sdl3_lib_handle,'SDL_InitSubSystem');
  Pointer(_SDL_QuitSubSystem):=GetProcedureAddress(sdl3_lib_handle,'SDL_QuitSubSystem');
  Pointer(_SDL_WasInit)      :=GetProcedureAddress(sdl3_lib_handle,'SDL_WasInit');
  Pointer(_SDL_free)         :=GetProcedureAddress(sdl3_lib_handle,'SDL_free');
  Pointer(_SDL_PollEvent)    :=GetProcedureAddress(sdl3_lib_handle,'SDL_PollEvent');
  Pointer(_SDL_WaitEvent)    :=GetProcedureAddress(sdl3_lib_handle,'SDL_WaitEvent');
 end;

 Result:=False;

 if first_init then
 begin
  if (_SDL_Init<>nil) then
  begin
   Result:=_SDL_Init(flags);
  end;
 end else
 begin
  if (_SDL_InitSubSystem<>nil) then
  begin
   Result:=_SDL_InitSubSystem(flags);
  end;
 end;

 if not Result then
 begin
  UnloadLibrary(sdl3_lib_handle);
  sdl3_lib_handle:=NilHandle;
  Exit;
 end;

 //.....
end;

procedure SDL_QuitSubSystem(flags: Integer);
var
 i:Integer;
begin
 if (sdl3_lib_handle=NilHandle) then Exit;

 if (_SDL_QuitSubSystem<>nil) then
 begin
  _SDL_QuitSubSystem(flags);
 end;

 i:=SDL_WasInit(SDL_INIT_AUDIO    or
                SDL_INIT_VIDEO    or
                SDL_INIT_JOYSTICK or
                SDL_INIT_HAPTIC   or
                SDL_INIT_GAMEPAD  or
                SDL_INIT_EVENTS   or
                SDL_INIT_SENSOR   or
                SDL_INIT_CAMERA
               );

 if (i=0) then
 begin
  UnloadLibrary(sdl3_lib_handle);
  sdl3_lib_handle:=NilHandle;

  Pointer(_SDL_Init)         :=nil;
  Pointer(_SDL_InitSubSystem):=nil;
  Pointer(_SDL_QuitSubSystem):=nil;
  Pointer(_SDL_WasInit)      :=nil;
  Pointer(_SDL_free)         :=nil;
  Pointer(_SDL_PollEvent)    :=nil;
  Pointer(_SDL_WaitEvent)    :=nil;
 end;

 //.....
end;

//

end.

