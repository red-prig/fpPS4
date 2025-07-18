unit form_filler;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 SysUtils,
 Controls,
 StdCtrls,
 TypInfo,
 Rtti,
 game_info;

type
 TFormDataProvider=class
  procedure SetText   (control:TComponent;const Text:RawByteString); virtual; abstract;
  function  GetText   (control:TComponent):RawByteString;            virtual; abstract;
  procedure SetInteger(control:TComponent;i:Integer);                virtual; abstract;
  function  GetInteger(control:TComponent):Integer;                  virtual; abstract;
  procedure SetBool   (control:TComponent;B:Boolean);                virtual; abstract;
  function  GetBool   (control:TComponent):Boolean;                  virtual; abstract;
  procedure SetClass  (control:TComponent;Obj:TObject);              virtual; abstract;
  procedure GetClass  (control:TComponent;Obj:TObject);              virtual; abstract;
 end;

type
 TMyControl=class(TControl)
  public
   property Text;
 end;

 TMyButtonControl=class(TButtonControl)
  public
   property Checked;
 end;

procedure PageLoad(Parent:TComponent;
                   const TabName:RawByteString;
                   Provider:TFormDataProvider;
                   src:TAbstractObject);

procedure FormLoad(Parent:TComponent;
                    Provider:TFormDataProvider;
                    src:TAbstractObject);

procedure PageSave(Parent:TComponent;
                   const TabName:RawByteString;
                   Provider:TFormDataProvider;
                   dst:TAbstractObject);

procedure FormSave(Parent:TComponent;
                   Provider:TFormDataProvider;
                   dst:TAbstractObject);

implementation

procedure PageLoad(Parent:TComponent;
                   const TabName:RawByteString;
                   Provider:TFormDataProvider;
                   src:TAbstractObject);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 TypeKind:TTypeKind;

 cname:RawByteString;
 control:TComponent;
begin

 i:=src.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   cname:='Edt_'+TabName+'_'+p.Name;
   control:=Parent.FindComponent(cname);
   Assert(control<>nil,'Control:'+cname+' not found!');
   if (control=nil) then Exit;

   TypeKind:=p.PropertyType.TypeKind;
   case TypeKind of

    tkSString,
    tkLString,
    tkAString:
     begin
      Provider.SetText(control,p.GetValue(src).AsString);
     end;

    tkInteger:
     begin
      Provider.SetInteger(control,p.GetValue(src).AsInteger);
     end;

    tkBool:
     begin
      Provider.SetBool(control,p.GetValue(src).AsBoolean);
     end;

    tkClass:
     begin
      Provider.SetClass(control,p.GetValue(src).AsObject);
     end;

    else
     Assert(false);
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

procedure FormLoad(Parent:TComponent;
                   Provider:TFormDataProvider;
                   src:TAbstractObject);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj:TObject;
begin
 i:=src.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   case p.PropertyType.TypeKind of

    tkClass:
     begin
      obj:=p.GetValue(src).AsObject;

      if (obj<>nil) then
      if obj.InheritsFrom(TAbstractObject) then
      begin
       PageLoad(Parent,p.Name,Provider,TAbstractObject(obj));
      end;
     end;

    else;
   end;

   i.Next;
  end;
 finally
  i.free;
 end;

end;

//////////////

procedure PageSave(Parent:TComponent;
                   const TabName:RawByteString;
                   Provider:TFormDataProvider;
                   dst:TAbstractObject);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 TypeKind:TTypeKind;

 cname:RawByteString;
 control:TComponent;
begin

 i:=dst.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   cname:='Edt_'+TabName+'_'+p.Name;
   control:=Parent.FindComponent(cname);
   Assert(control<>nil,cname);
   if (control=nil) then Exit;

   TypeKind:=p.PropertyType.TypeKind;
   case TypeKind of

    tkSString,
    tkLString,
    tkAString:
     begin
      p.SetValue(dst,Provider.GetText(control));
     end;

    tkInteger:
     begin
      p.SetValue(dst,Provider.GetInteger(control));
     end;

    tkBool:
     begin
      p.SetValue(dst,Provider.GetBool(control));
     end;

    tkClass:
     begin
      Provider.GetClass(control,p.GetValue(dst).AsObject);
     end;

    else
     Assert(false);
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;

procedure FormSave(Parent:TComponent;
                   Provider:TFormDataProvider;
                   dst:TAbstractObject);
var
 i:TRttiPropertyIterator;
 p:TRttiProperty;
 obj:TObject;
begin
 i:=dst.GetPropertyIterator;
 try
  while (i.GetProperty<>nil) do
  begin

   p:=i.GetProperty;

   case p.PropertyType.TypeKind of

    tkClass:
     begin
      obj:=p.GetValue(dst).AsObject;

      if (obj<>nil) then
      if obj.InheritsFrom(TAbstractObject) then
      begin
       PageSave(Parent,p.Name,Provider,TAbstractObject(obj));
      end;
     end;

    else;
   end;

   i.Next;
  end;
 finally
  i.free;
 end;
end;


end.

