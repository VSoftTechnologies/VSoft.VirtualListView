unit DemoMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  VSoftVirtualListView, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label2: TLabel;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure VListViewPaintNoRows(const Sender: TObject; const ACanvas: TCanvas; const paintRect: TRect);
    procedure VListViewPaintRow(const Sender: TObject; const ACanvas: TCanvas; const itemRect: TRect; const index: Int64; const state: TPaintRowState);
    procedure VListViewRowChange(const Sender: TObject; const newRowIndex: Int64; const direction: TScrollDirection; const delta: Int64);
    procedure Button4Click(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    FPackageName : string;
    FMouseWheeControl: TControl;
    FVirtualListView: TVSoftVirtualListView;


    FPackageNameFontSize : integer;
    FPadding : integer;
    FAuthorOffsetX : integer;
    FFontSize : integer;
  protected
  public
    procedure MouseWheelHandler(var Message: TMessage); override;

  end;

var
  Form2: TForm2;

implementation

uses
  System.Types;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  FVirtualListView.RowCount := 0;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  FVirtualListView.RowCount := 200000;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  FPackageName := 'VSoft.DUnitX';
  FVirtualListView.InvalidateRow(4);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  FVirtualListView.ScrollInView(FVirtualListView.CurrentRow);
end;


procedure TForm2.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  FPackageNameFontSize := MulDiv(FPackageNameFontSize, NewDpi, OldDPI);
  FPadding := MulDiv(FPadding, NewDPI, OldDPI);
  FAuthorOffsetX := MulDiv(FAuthorOffsetX, NewDPI, OldDPI);
  FFontSize := MulDiv(FFontSize, NewDPI, OldDPI);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FPackageNameFontSize := 11;
  FPadding := 3;
  FAuthorOffsetX := 175;
  FFontSize := Self.Font.Size;
  FVirtualListView := TVSoftVirtualListView.Create(Self);
  FVirtualListView.Align := alClient;
  FVirtualListView.BorderStyle := bsNone;
  FVirtualListView.BevelOuter := TBevelCut.bvLowered;
  FVirtualListView.BevelInner := TBevelCut.bvNone;
  FVirtualListView.BevelKind := TBevelKind.bkFlat;
  FVirtualListView.DoubleBuffered := true;
  FVirtualListView.RowHeight := 80;
  FVirtualListView.RowCount := 200;
  FVirtualListView.Parent := Self;
  FVirtualListView.TabStop := true;
  FVirtualListView.TabOrder := 0;
  FVirtualListView.OnPaintRow := Self.VListViewPaintRow;
  FVirtualListView.OnPaintNoRows := Self.VListViewPaintNoRows;
  FVirtualListView.OnRowChange := Self.VListViewRowChange;
  FPackageName := 'VSoft.Awaitable';

end;

//https://stackoverflow.com/questions/2251019/how-to-direct-the-mouse-wheel-input-to-control-under-cursor-instead-of-focused
procedure TForm2.MouseWheelHandler(var Message: TMessage);
var
  Control: TControl;
begin
  Control := ControlAtPos(ScreenToClient(SmallPointToPoint(TWMMouseWheel(Message).Pos)), False, True, True);
  if Assigned(Control) and (Control <> ActiveControl) and (Control <> FMouseWheeControl) then
  begin
    FMouseWheeControl := Control;  //dealing with stack overflow.

    Message.Result := Control.Perform(CM_MOUSEWHEEL, Message.WParam, Message.LParam);
    if Message.Result = 0 then
      Control.DefaultHandler(Message);
    FMouseWheeControl := nil;
  end
  else
    inherited MouseWheelHandler(Message);
end;


procedure TForm2.VListViewPaintNoRows(const Sender: TObject; const ACanvas: TCanvas; const paintRect: TRect);
begin
  ACanvas.TextOut(10,10, 'No packages found');
end;

procedure TForm2.VListViewPaintRow(const Sender: TObject; const ACanvas: TCanvas; const itemRect: TRect; const index: Int64; const state: TPaintRowState);
var
  focusRect : TRect;
begin
  if index > FVirtualListView.RowCount -1  then
    raise Exception.Create('Invalid Row');
  ACanvas.Brush.Style := bsSolid;
  if (state in [rsFocusedSelected, rsFocusedHot, rsHot]) then
  begin
    ACanvas.Brush.Color := clSkyBlue;
    ACanvas.Font.Color := clWindowText;
  end
  else
  begin
    ACanvas.Brush.Color := Self.Color;
    ACanvas.Font.Color := clWindowText;
  end;

  ACanvas.FillRect(itemRect);
  ACanvas.Font.Size := FPackageNameFontSize;
  if index = 4 then
    ACanvas.TextOut(itemRect.Left + FPadding, itemRect.Top + FPadding, FPackageName + '  ' + IntToStr(index))
  else
    ACanvas.TextOut(itemRect.Left + FPadding, itemRect.Top + FPadding, 'VSoft.Awaitable  ' + IntToStr(index) );

  ACanvas.Font.Size := FFontSize;
  ACanvas.TextOut(itemRect.Left + FAuthorOffsetX, itemRect.Top + FPadding * 2, 'by Vincent Parrett');
  ACanvas.TextOut(itemRect.Left + FPAdding * 2, itemRect.Top + FPadding * 7, 'This is Row : ' + IntToStr(index) + ' descriptions of some sort');

  if state in [rsFocusedSelected, rsSelected] then
  begin
    focusRect := itemRect;
    InflateRect(focusRect, -FPadding, -FPadding);
    DrawFocusRect(ACanvas.Handle, focusRect)
  end;
end;

procedure TForm2.VListViewRowChange(const Sender: TObject; const newRowIndex: Int64; const direction: TScrollDirection; const delta: Int64);
begin
  Label1.Caption := 'Current (Event) : ' + IntToStr(newRowIndex) + ' Direction : ' + IntToStr(delta) ;
  Label2.Caption := 'Current Row : ' + IntToStr(FVirtualListView.CurrentRow) + ' Top Row : ' + IntToStr(FVirtualListView.TopRow);
end;

end.
