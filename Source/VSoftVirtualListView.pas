unit VSoftVirtualListView;

interface

uses
  System.Types,
  System.Classes,
  System.Generics.Collections,
  WinApi.Windows,
  WinApi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms;

type

  TScrollDirection = (sdUp, sdDown);

  TPaintRowState = (rsNormal, rsHot, rsSelected, rsFocusedNormal, rsFocusedHot, rsFocusedSelected);

  TPaintRowEvent = procedure(const Sender : TObject; const ACanvas : TCanvas; const itemRect : TRect; const index : Int64; const state : TPaintRowState) of object;
  TPaintNoRowsEvent = procedure(const Sender : TObject; const ACanvas : TCanvas; const paintRect : TRect) of object;
  TRowChangeEvent = procedure(const Sender : TObject; const newRowIndex : Int64; const direction : TScrollDirection; const delta : Int64 ) of object;

  TVSoftVirtualListView = class(TCustomControl)
  private
    FBorderStyle : TBorderStyle;

    FOnPaintRow : TPaintRowEvent;
    FOnPaintNoRows : TPaintNoRowsEvent;
    FOnRowChangeEvent : TRowChangeEvent;

    FUpdating : boolean;

    FRowCount : Int64;
    FRowHeight : integer;

    FVisibleRows : integer; //number of rows we can see
    FSelectableRows : integer; //number of rows we can click on
    FTopRow : Int64; //this is the cursor in all the rows.
    FCurrentRow : Int64; //this is our cursor within the visible rows

    FHoverRow : integer; //mouse over

    FRowRects : TList<TRect>;

    FScrollPos : Int64;
    FScrollBarVisible : boolean;
    procedure SetBorderStyle(const Value: TBorderStyle);

  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure UpdateScrollBar;

    procedure DoRowChanged(const oldTopRow, oldCurrentRow : integer);

    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;


    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;


    procedure DoLineUp;
    procedure DoLineDown;
    procedure DoPageUp(const fromScrollBar : boolean; const newScrollPostition : integer);
    procedure DoPageDown(const fromScrollBar : boolean; const newScrollPostition : integer);
    procedure DoGoTop;
    procedure DoGoBottom;
    procedure DoTrack(const newScrollPostition : integer);

    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;  var ScrollPos: Integer);

    procedure UpdateVisibleRows;

    procedure SetRows(const Value: Int64);
    procedure SetRowHeight(const Value: integer);

    function GetRowPaintState(const rowIdx : integer) : TPaintRowState;


    procedure DoOnPaintRow(const itemRect : TRect; const index : Int64; const state : TPaintRowState);
    procedure Paint;override;
    procedure Resize; override;

    function GetRowFromY(const Y : integer) : integer;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;


    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure CreateHandle; override;

    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;


  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    class constructor Create;
    procedure InvalidateRow(const index : Int64);
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clWindow;
    property Enabled;
    property Font;
    property Height default 100;
    property ParentBackground;
    property ParentColor;
    property StyleElements;
    property TabOrder;
    property TabStop default True;
    property Width default 100;
    property Visible;


    property RowCount : Int64 read FRowCount write SetRows;
    property RowHeight : integer read FRowHeight write SetRowHeight;
    property OnPaintRow : TPaintRowEvent read FOnPaintRow write FOnPaintRow;
    property OnPaintNoRows : TPaintNoRowsEvent read FOnPaintNoRows write FOnPaintNoRows;
    property OnRowChange : TRowChangeEvent read FOnRowChangeEvent write FOnRowChangeEvent;
  end;


implementation

uses
  System.Math,
  Vcl.Themes,
  Vcl.Styles;

{ TVSoftVirtualListView }

procedure TVSoftVirtualListView.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;

procedure TVSoftVirtualListView.CMEnter(var Message: TCMEnter);
begin
  inherited;
  Invalidate;
end;

procedure TVSoftVirtualListView.CMExit(var Message: TCMExit);
begin
  FHoverRow := -1;
  Invalidate;
  inherited;
end;

procedure TVSoftVirtualListView.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

procedure TVSoftVirtualListView.CMMouseLeave(var Msg: TMessage);
var
  oldHoverRow : integer;
  rowState : TPaintRowState;
begin
  oldHoverRow := FHoverRow;
  FHoverRow := -1;
  if (oldHoverRow <> -1) and (oldHoverRow < FVisibleRows)  and (oldHoverRow < FRowCount) and ((oldHoverRow + FTopRow) < FRowCount) then
  begin
    rowState := GetRowPaintState(oldHoverRow);
    DoOnPaintRow(FRowRects[oldHoverRow], FTopRow + oldHoverRow, rowState);
  end;
  inherited;
end;

constructor TVSoftVirtualListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  FScrollBarVisible := false;
  FRowRects := TList<TRect>.Create;
  ControlStyle := [csDoubleClicks, csCaptureMouse, csDisplayDragImage, csClickEvents, csPannable];
  FScrollPos := 0;
  FHoverRow := -1;
  FRowHeight := 40;
  TabStop := true;
  ParentBackground := true;
  ParentColor := true;
  ParentDoubleBuffered := true;
end;

class constructor TVSoftVirtualListView.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TVSoftVirtualListView, TScrollingStyleHook);
end;

procedure TVSoftVirtualListView.CreateHandle;
begin
  inherited CreateHandle;
  Resize;
end;

procedure TVSoftVirtualListView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if (FBorderStyle = bsSingle) then
  begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
  Params.Style := Params.Style + WS_VSCROLL;
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);

end;

procedure TVSoftVirtualListView.CreateWnd;
begin
  inherited;
  UpdateVisibleRows;
  Invalidate;
end;

destructor TVSoftVirtualListView.Destroy;
begin
  FRowRects.Free;
  inherited;
end;


function TVSoftVirtualListView.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  scrollPos : integer;
begin
  scrollPos := Min(FScrollPos + 1, FRowCount - 1) ;
  ScrollBarScroll(Self,TScrollCode.scLineDown, scrollPos );
  result := true;
end;

function TVSoftVirtualListView.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  scrollPos : integer;
begin
  scrollPos := Max(0, FScrollPos - 1);
  ScrollBarScroll(Self,TScrollCode.scLineUp, scrollPos );
  result := true;
end;

procedure TVSoftVirtualListView.DoOnPaintRow(const itemRect: TRect; const index: Int64; const state: TPaintRowState);
begin
  if HandleAllocated then
  begin
//    Canvas.Brush.Color := StyleServices.GetSystemColor(clWindow);
//    Canvas.FillRect(itemRect);
    if Assigned(FOnPaintRow) then
      FOnPaintRow(Self, Canvas, itemRect, index, state);
  end;
end;

procedure TVSoftVirtualListView.DoRowChanged(const oldTopRow, oldCurrentRow : integer);
var
  oldIdx : integer;
  newIdx : integer;
  delta : integer;
  direction : TScrollDirection;
begin
  if not Assigned(FOnRowChangeEvent) then
    exit;

  oldIdx := oldTopRow + oldCurrentRow;
  newIdx := FTopRow + FCurrentRow;
  delta := newIdx - oldIdx;
  if delta = 0 then
    exit
  else if delta < 0 then
    direction := TScrollDirection.sdUp
  else
    direction := TScrollDirection.sdDown;

  FOnRowChangeEvent(Self,FTopRow + FCurrentRow, direction, delta);
end;

procedure TVSoftVirtualListView.DoTrack(const newScrollPostition: integer);
var
  oldTopRow : integer;
  oldCurrentRow : integer;
  delta : integer;
begin
  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  FTopRow := newScrollPostition;
  delta := FTopRow - oldTopRow;
  if delta <> 0 then
  begin
    if delta > 0 then //scrolling down
      FCurrentRow := Max(0, oldCurrentRow - delta)
    else
      FCurrentRow := Min(FSelectableRows -1, oldCurrentRow - delta);


    Invalidate;
    DoRowChanged(oldTopRow, oldCurrentRow);
  end;


end;

function TVSoftVirtualListView.GetRowFromY(const Y: integer): integer;
begin
  result := (Y div FRowHeight); //this is probably not quite right.

end;

function TVSoftVirtualListView.GetRowPaintState(const rowIdx: integer): TPaintRowState;
begin
  if Self.Focused then
  begin
    result := rsFocusedNormal;
    if (rowIdx = FCurrentRow) then
      result := rsFocusedSelected
    else if rowIdx = FHoverRow then
      result := rsFocusedHot;
  end
  else
  begin
    result := rsNormal;
    if (rowIdx = FCurrentRow) then
      result := rsSelected
    else if rowIdx = FHoverRow then
      result := rsHot;
  end;
end;

procedure TVSoftVirtualListView.InvalidateRow(const index: Int64);
var
  rowState : TPaintRowState;
  row : integer;
begin
  //work out if the row is actually visible;
  if FRowCount = 0 then
    exit;
  if (index < 0) or (index > FRowCount) then
    exit;

  if (index >= FTopRow) and (index < FTopRow + FVisibleRows) then
  begin
    rowState := GetRowPaintState(index - FTopRow);
    DoOnPaintRow(FRowRects[index - FTopRow], index, rowState);
  end;
end;

procedure TVSoftVirtualListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP,VK_LEFT: DoLineUp;
    VK_DOWN, VK_RIGHT: DoLineDown;
    VK_NEXT: DoPageDown(false,0);
    VK_PRIOR: DoPageUp(false, 0);
    VK_HOME: DoGoTop;
    VK_END: DoGoBottom;
  end;
end;

procedure TVSoftVirtualListView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  row : Integer;
  oldRow : integer;
begin
  inherited;
  SetFocus;
  row := GetRowFromY(Y);
  if  row <> FCurrentRow then
  begin
    oldRow := FCurrentRow;
    FCurrentRow := row;
    DoRowChanged(FTopRow, oldRow);
    Invalidate;
  end;

end;

procedure TVSoftVirtualListView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  row : Integer;
  oldHoverRow : integer;
  rowState : TPaintRowState;
begin
  inherited;
  row := GetRowFromY(Y);
  if row <> FHoverRow then
  begin
    oldHoverRow := FHoverRow;
    FHoverRow := row;
    if (oldHoverRow <> -1) and (oldHoverRow < FVisibleRows)  and (oldHoverRow < FRowCount) and ((oldHoverRow + FTopRow) < FRowCount) then
    begin
      rowState := GetRowPaintState(oldHoverRow);
      DoOnPaintRow(FRowRects[oldHoverRow], FTopRow + oldHoverRow, rowState);
    end;
    if (FHoverRow > -1) and (FHoverRow < FVisibleRows ) and (FHoverRow < FRowCount) and ((FHoverRow + FTopRow) < FRowCount)  then
    begin
      rowState := GetRowPaintState(FHoverRow);
      DoOnPaintRow(FRowRects[FHoverRow], FTopRow + FHoverRow , rowState);
    end
  end;
end;


procedure TVSoftVirtualListView.Paint;
var
  i: Integer;
  r : TRect;
  rowIdx : integer;

  LCanvas : TCanvas;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  LColor : TColor;
  rowState : TPaintRowState;
  buffer : TBitmap;
begin
  Buffer := nil;
  try
    if not DoubleBuffered and TStyleManager.IsCustomStyleActive and (seClient in StyleElements) then
    begin
      Buffer := TBitmap.Create;
      Buffer.SetSize(ClientWidth, ClientHeight);
      LCanvas := Buffer.Canvas;
    end
    else
      LCanvas := Canvas;

    LStyle := StyleServices;

    if LStyle.Enabled and (seClient in StyleElements) then
    begin
      LDetails := LStyle.GetElementDetails(tpPanelBackground);
      if not LStyle.GetElementColor(LDetails, ecFillColor, LColor) or (LColor = clNone) then
        LColor := Color;
    end
    else
      LColor := Color;

    r := GetClientRect;

    LCanvas.Brush.Style := bsSolid;
    LCanvas.Brush.Color := LColor;
    LCanvas.FillRect(r);

    if (FRowCount = 0) and Assigned(FOnPaintNoRows) then
      FOnPaintNoRows(Self, LCanvas, r)
    else if (FRowCount > 0) and Assigned(FOnPaintRow) then
    begin
      for i := 0 to FVisibleRows - 1 do
      begin
        rowIdx := FTopRow + i;
        if rowIdx >= FRowCount then
          exit;
        r := FRowRects[i];

        rowState := GetRowPaintState(i);

        FOnPaintRow(Self, LCanvas, r, rowIdx, rowState);
      end;
    end;

  finally
    if Buffer <> nil then
    begin
      Canvas.Draw(0, 0, Buffer);
      Buffer.Free;
    end;

  end;
end;



procedure TVSoftVirtualListView.Resize;
var
  oldCurrentRow : integer;
  oldTopRow : integer;
begin
  inherited;
  if (not HandleAllocated) then
    Exit;

  if csDestroying in ComponentState then
    Exit;

  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;

  FHoverRow := -1;
  UpdateVisibleRows;

  //make sure we show as much as possible
  if FCurrentRow > FSelectableRows then
    FCurrentRow := FSelectableRows;

  if FScrollPos = FRowCount -1 then
    FTopRow := Max(0, FRowCount - FSelectableRows)
  else if (FVisibleRows = FRowCount) and (FTopRow > 0) then
    FTopRow := 0
  else if FTopRow > 0 then
  begin
    if FRowCount < FTopRow + FSelectableRows then
       FTopRow := Max(0,FRowCount - FSelectableRows);
  end;

  if sfHandleMessages in StyleServices.Flags then
    SendMessage(Handle, WM_NCPAINT, 0, 0);

  if (oldTopRow <> FTopRow) or (oldCurrentRow <> FCurrentRow) then
    DoRowChanged(oldCurrentRow, oldCurrentRow);

end;

procedure TVSoftVirtualListView.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  case scrollCode of
    TScrollCode.scLineUp: DoLineUp;
    TScrollCode.scLineDown: DoLineDown;
    TScrollCode.scPageUp:
    begin
      ScrollPos :=  FScrollPos - FSelectableRows;
      if ScrollPos < 0 then
        ScrollPos := 0;
      DoPageUp(true,ScrollPos);
    end;
    TScrollCode.scPageDown:
    begin
      ScrollPos :=  FScrollPos + FSelectableRows;
      if ScrollPos > FRowCount -1 then
        ScrollPos := FRowCount - 1;
      DoPageDown(true, ScrollPos);
    end;
    TScrollCode.scPosition:;
    TScrollCode.scTrack:
    begin
      DoTrack(ScrollPos);
    end;
    TScrollCode.scTop: DoGoTop;
    TScrollCode.scBottom: DoGoBottom;
    TScrollCode.scEndScroll: exit ;
  end;
end;

procedure TVSoftVirtualListView.DoLineDown;
var
  oldCurrentRow : integer;
  oldTopRow : integer;
  pageSize : integer;
  rowState : TPaintRowState;
begin
  oldCurrentRow := FCurrentRow;
  oldTopRow := FTopRow;
  if (FCurrentRow < FSelectableRows - 1) and (FCurrentRow < FRowCount -1) then
  begin
    //easy.
    Inc(FCurrentRow);
    rowState := GetRowPaintState(oldCurrentRow);
    DoOnPaintRow(FRowRects[oldCurrentRow], FTopRow + oldCurrentRow, rowState);
    rowState := GetRowPaintState(FCurrentRow);
    DoOnPaintRow(FRowRects[FCurrentRow], FTopRow + FCurrentRow , rowState);
  end
  else if FRowCount > FSelectableRows then
  begin
    //more complicated.
    pageSize := Min(FSelectableRows, FRowCount -1);
    FTopRow := Min(FTopRow + 1,FRowCount - pageSize);
    if oldTopRow <> FTopRow then
    begin
       //full paint.
       Invalidate;
    end;
  end;
  DoRowChanged(oldTopRow, oldCurrentRow);

  FScrollPos := FTopRow;
//  if FVertScrollBar.Visible then
//    FVertScrollBar.Position := FTopRow;
  UpdateScrollBar;
end;

procedure TVSoftVirtualListView.DoPageDown(const fromScrollBar : boolean; const newScrollPostition : integer);
var
  oldCurrentRow : integer;
  oldTopRow : integer;
  pageSize : integer;
  rowState : TPaintRowState;
  fullRepaint : boolean;
  delta : integer;
begin
  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  fullRepaint := false;
  delta := 0;
  if fromScrollBar then
  begin
    FTopRow := newScrollPostition;
    if FTopRow > (FRowCount - FSelectableRows) then
      FTopRow := FRowCount - FSelectableRows;
    FCurrentRow := FSelectableRows - 1;
    if oldTopRow <> FTopRow then
    begin
      delta := FTopRow - oldTopRow;
      fullRepaint := true;
    end
    else if oldCurrentRow <> FCurrentRow then
      delta := FCurrentRow  - oldCurrentRow;
  end
  else
  begin
    if FCurrentRow < FSelectableRows - 1 then
    begin
      FCurrentRow := FSelectableRows -1;
      delta := FCurrentRow  - oldCurrentRow;
    end
    else if FRowCount > FSelectableRows then
    begin
      pageSize := Min(FSelectableRows, FRowCount -1);
      FTopRow := Min(FTopRow + pageSize, FRowCount - pageSize);
      if oldTopRow <> FTopRow then
      begin
         delta := FTopRow - oldTopRow;
         fullRepaint := true;
      end;
    end;
  end;
  if delta > 0 then
  begin
    if not fullRepaint then
    begin
      rowState := GetRowPaintState(oldCurrentRow);
      DoOnPaintRow(FRowRects[oldCurrentRow], FTopRow + oldCurrentRow, rowState);
      rowState := GetRowPaintState(FCurrentRow);
      DoOnPaintRow(FRowRects[FCurrentRow], FTopRow + FCurrentRow , rowState);
    end
    else
      Invalidate;

    DoRowChanged(oldTopRow, oldCurrentRow);
//    if FVertScrollBar.Visible then
//      FVertScrollBar.Position := FTopRow;
    FScrollPos := FTopRow;
    UpdateScrollBar;
  end;

end;

procedure TVSoftVirtualListView.DoGoBottom;
var
  oldTopRow : integer;
  oldCurrentRow : integer;
  rowState : TPaintRowState;
begin
  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  FCurrentRow := FSelectableRows - 1;
  FTopRow := FRowCount - FSelectableRows;
  FScrollPos := FRowCount -1;

  if FTopRow <> oldTopRow then
  begin
    //need a full repaint.
    invalidate;
  end
  else if FCurrentRow <> oldCurrentRow then
  begin
    rowState := GetRowPaintState(oldCurrentRow);
    DoOnPaintRow(FRowRects[oldCurrentRow], oldTopRow + oldCurrentRow, rowState);
    rowState := GetRowPaintState(FCurrentRow);
    DoOnPaintRow(FRowRects[FCurrentRow], FTopRow + FCurrentRow , rowState);
  end;
  DoRowChanged(oldTopRow, oldCurrentRow);

  //FVertScrollBar.Position := FVertScrollBar.Max;
  UpdateScrollBar;
end;

procedure TVSoftVirtualListView.DoGoTop;
var
  oldCurrentRow : integer;
  oldTopRow : integer;
  rowState : TPaintRowState;
begin
  oldCurrentRow := FCurrentRow;
  oldTopRow := FTopRow;
  if (oldTopRow <> 0) or (oldCurrentRow <> 0) then
  begin
    //some work to do.
    if oldTopRow = 0  then
    begin
      //no scrolling so we can just paint the rows that changed.
      FCurrentRow := 0;
      FTopRow := 0;
      rowState := GetRowPaintState(oldCurrentRow);
      DoOnPaintRow(FRowRects[oldCurrentRow], oldCurrentRow + oldTopRow, rowState);
      rowState := GetRowPaintState(0);
      DoOnPaintRow(FRowRects[0], 0 , rowState);
    end
    else
    begin
      FTopRow := 0;
      FCurrentRow := 0;
      Invalidate;
    end;
//    FVertScrollBar.Position := 0;
    FScrollPos := 0;
    UpdateScrollBar;
    DoRowChanged(oldTopRow, oldCurrentRow);
  end;
end;

procedure TVSoftVirtualListView.DoLineUp;
var
  oldTopRow : integer;
  oldCurrentRow : integer;
  rowState : TPaintRowState;
begin
  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  if FCurrentRow  > 0 then
    Dec(FCurrentRow)
  else if FTopRow > 0 then
    Dec(FTopRow);

  if FTopRow <> oldTopRow then
    Invalidate
  else
  begin
    rowState := GetRowPaintState(oldCurrentRow);
    DoOnPaintRow(FRowRects[oldCurrentRow], oldTopRow + oldCurrentRow, rowState);
    rowState := GetRowPaintState(FCurrentRow);
    DoOnPaintRow(FRowRects[FCurrentRow], FTopRow + FCurrentRow , rowState);
  end;
  FScrollPos := FTopRow;
//  FVertScrollBar.Position := FTopRow;
  UpdateScrollBar;
  DoRowChanged(oldTopRow, oldCurrentRow);
end;

procedure TVSoftVirtualListView.DoPageUp(const fromScrollBar : boolean; const newScrollPostition : integer);
var
  oldTopRow : integer;
  oldCurrentRow : integer;
  rowState : TPaintRowState;
  fullRepaint : boolean;
  delta : integer;
  pageSize : integer;
begin
  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  fullRepaint := false;
  delta := 0;
  if fromScrollBar then
  begin
    FTopRow := newScrollPostition;
    FCurrentRow := 0;
    if oldTopRow <> FTopRow then
    begin
      delta := FTopRow - oldTopRow;
      fullRepaint := true;
    end
    else if oldCurrentRow <> FCurrentRow then
      delta := FCurrentRow  - oldCurrentRow;
  end
  else
  begin
    if FCurrentRow > 0 then
    begin
      FCurrentRow := 0;
      delta := FCurrentRow  - oldCurrentRow;
    end
    else
    begin
      pageSize := Min(FSelectableRows, FRowCount -1);
      FTopRow := Max(0, FTopRow - pageSize);
      if FTopRow <> oldTopRow then
      begin
        fullRepaint := true;
        delta := FTopRow - oldTopRow;
      end;
    end;
  end;

  if delta < 0 then
  begin
    if not fullRepaint then
    begin
      rowState := GetRowPaintState(oldCurrentRow);
      DoOnPaintRow(FRowRects[oldCurrentRow], FTopRow + oldCurrentRow, rowState);
      rowState := GetRowPaintState(FCurrentRow);
      DoOnPaintRow(FRowRects[FCurrentRow], FTopRow + FCurrentRow , rowState);
    end
    else
      Invalidate;

    DoRowChanged(oldTopRow, oldCurrentRow);
//    if FVertScrollBar.Visible then
//      FVertScrollBar.Position := FTopRow;
    FScrollPos := FTopRow;
    UpdateScrollBar;
  end;
end;

procedure TVSoftVirtualListView.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TVSoftVirtualListView.SetRowHeight(const Value: integer);
begin
  if FRowHeight <> value then
  begin
    FRowHeight := Value;
    UpdateVisibleRows;
    Invalidate;
  end;
end;

procedure TVSoftVirtualListView.SetRows(const Value: Int64);
begin
  if FRowCount <> value then
  begin
    if value < (FTopRow + FCurrentRow)  then
    begin
      FCurrentRow := 0;
      FTopRow := 0;
      FScrollPos := 0;
    end;
    FRowCount := Value;

    if HandleAllocated then
    begin
      UpdateVisibleRows;
      Invalidate;
    end;
  end;
end;

procedure TVSoftVirtualListView.UpdateScrollBar;
var
  sbInfo : TScrollInfo;
  prevVisible : boolean;
begin
  if not HandleAllocated then
    exit;

  prevVisible := FScrollBarVisible;
  sbInfo.cbSize := SizeOf(TScrollInfo);
  sbInfo.fMask := SIF_ALL;
    sbInfo.nMin := 0;

  //Note : this may trigger a resize if the visibility changes
  if FRowCount <= FSelectableRows  then
  begin
    FScrollBarVisible := false;
    sbInfo.nMax := 0;
    sbInfo.nPage := 0;
    sbInfo.nPos := 0;
    SetScrollInfo(Handle, SB_VERT, sbInfo, True);
  end
  else
  begin
    FScrollBarVisible := true;
    sbInfo.nMax := Max(FRowCount -1, 0);
    sbInfo.nPage := Min(FSelectableRows, FRowCount -1);
    sbInfo.nPos := Min(FScrollPos, FRowCount -1) ;
    SetScrollInfo(Handle, SB_VERT, sbInfo, True);
  end;
  if prevVisible <> FScrollBarVisible then
    Invalidate;
end;

procedure TVSoftVirtualListView.UpdateVisibleRows;
var
  i: Integer;
  r : TRect;
  offset : integer;
begin
  FHoverRow := -1;
  if FUpdating then
    exit;
  FUpdating := true;
  if HandleAllocated then
  begin
    FVisibleRows :=  ClientHeight div RowHeight;
    FSelectableRows := FVisibleRows; //the number of full rows
    if (FRowCount > FVisibleRows) and (ClientHeight mod RowHeight > 0) then
    begin
      //add 1 to ensure a partial row is painted.
      FVisibleRows  := FVisibleRows + 1;
    end;

    FRowRects.Clear;
    offset := 0;
    r := GetClientRect;
    //pre caculate the row rect's to avoid doing it in paint
    for i := 0 to FVisibleRows -1 do
    begin
      r.Top := offset + (i * FRowHeight);
      if i < FSelectableRows then
        r.Height := FRowHeight
      else
        r.Height := ClientHeight - r.Top;
      FRowRects.Add(r);
    end;
    UpdateScrollBar;
  end;
  FUpdating := false;
end;


procedure TVSoftVirtualListView.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1; //we will paint the background
end;

procedure TVSoftVirtualListView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;


procedure TVSoftVirtualListView.WMSize(var Message: TWMSize);
begin
  inherited;
  //force repaint during resizing rather than just after.
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
end;


procedure TVSoftVirtualListView.WMVScroll(var Message: TWMVScroll);
var
  p : Integer;
begin
  with Message do
  begin
    if ScrollCode = 8 then
      exit;
    p := pos;
    Self.ScrollBarScroll(Self, TScrollCode(ScrollCode), p);
  end;
end;

end.
