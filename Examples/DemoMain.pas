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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FVirtualListView : TVSoftVirtualListView;
    FPackageName : string;
    FMouseWheeControl: TControl;
  protected
    procedure PaintRow(const Sender : TObject; const ACanvas : TCanvas; const itemRect : TRect; const index : Int64; const rowState : TPaintRowState);
    procedure PaintNoRows(const Sender : TObject; const ACanvas : TCanvas; const paintRect : TRect);
    procedure RowChanged(const Sender : TObject; const newRowIndex : Int64; const direction : TScrollDirection; const delta : Int64);

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
  FVirtualListView.RowCount := 200;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  FPackageName := 'VSoft.DUnitX';
  FVirtualListView.InvalidateRow(4);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FVirtualListView := TVSoftVirtualListView.Create(Self);
  FVirtualListView.Align := alClient;
  FVirtualListView.BorderStyle := bsNone;
  FVirtualListView.BevelOuter := TBevelCut.bvLowered;
  FVirtualListView.BevelInner := TBevelCut.bvNone;
  FVirtualListView.BevelKind := TBevelKind.bkFlat;
  FVirtualListView.DoubleBuffered := true;
  FVirtualListView.RowHeight := 80;
  FVirtualListView.RowCount := 20;
  FVirtualListView.OnPaintRow := Self.PaintRow;
  FVirtualListView.OnPaintNoRows := Self.PaintNoRows;
  FVirtualListView.OnRowChange := Self.RowChanged;
  FVirtualListView.Parent := Self;
  FVirtualListView.TabStop := true;
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

procedure TForm2.PaintNoRows(const Sender: TObject; const ACanvas: TCanvas; const paintRect: TRect);
begin
  ACanvas.TextOut(10,10, 'No packages found');
end;

procedure TForm2.PaintRow(const Sender: TObject; const ACanvas: TCanvas; const itemRect: TRect; const index: Int64; const rowState: TPaintRowState);
var
  oldSize : integer;
  focusRect : TRect;
begin
  if index > FVirtualListView.RowCount -1  then
    raise Exception.Create('Invalid Row');
  ACanvas.Brush.Style := bsSolid;
  if (rowstate in [rsFocusedSelected, rsFocusedHot, rsHot]) then
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
//  ACanvas.Brush.Style:=bsClear;
  oldSize := ACanvas.Font.Size;
  ACanvas.Font.Size := 11;
  if index = 4 then
    ACanvas.TextOut(itemRect.Left + 3, itemRect.Top + 2, FPackageName + '  ' + IntToStr(index))
  else
    ACanvas.TextOut(itemRect.Left + 3, itemRect.Top + 2, 'VSoft.Awaitable  ' + IntToStr(index) );

  ACanvas.Font.Size := oldSize;
  ACanvas.TextOut(itemRect.Left + 150, itemRect.Top + 5, 'by Vincent Parrett');
  ACanvas.TextOut(itemRect.Left + 5, itemRect.Top + 20, 'This is Row : ' + IntToStr(index) + ' descriptions of some sort');

  if rowState in [rsFocusedSelected, rsSelected] then
  begin
    focusRect := itemRect;
    InflateRect(focusRect, -2, -2);
    DrawFocusRect(ACanvas.Handle, focusRect)
  end;
end;

procedure TForm2.RowChanged(const Sender: TObject; const newRowIndex: Int64; const direction: TScrollDirection; const delta: Int64);
begin
   Label1.Caption := 'Current (Event) : ' + IntToStr(newRowIndex) + ' Direction : ' + IntToStr(delta) ;
   Label2.Caption := 'Current Row : ' + IntToStr(FVirtualListView.CurrentRow) + ' Top Row : ' + IntToStr(FVirtualListView.TopRow);
end;

end.
