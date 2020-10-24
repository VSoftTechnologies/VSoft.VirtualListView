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
    FSelectableRows : integer; //number of rows we can click on (full rows)

    FTopRow : Int64; //this is the top row cursor .
    FCurrentRow : Int64; //this is our currently selected row.

    FHoverRow : integer; //mouse over row in view (add to top row to get index)

    FRowRects : TList<TRect>;

    FScrollPos : Int64;
    FScrollBarVisible : boolean;
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetCurrentRow(const Value: Int64);

  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure UpdateScrollBar;

    function RowInView(const row : Int64) : boolean;
    function GetViewRow(const row : Int64) : integer;
    function IsAtTop : boolean;
    function IsAtEnd : boolean;

    procedure DoRowChanged(const oldCurrentRow : integer);

    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;


    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;


    procedure DoLineUp(const fromScrollBar : boolean);
    procedure DoLineDown(const fromScrollBar : boolean);
    procedure DoPageUp(const fromScrollBar : boolean; const newScrollPostition : integer);
    procedure DoPageDown(const fromScrollBar : boolean; const newScrollPostition : integer);
    procedure DoGoTop;
    procedure DoGoBottom;
    procedure DoTrack(const newScrollPostition : integer);

    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;  var ScrollPos: Integer);

    procedure UpdateVisibleRows;

    procedure SetRows(const Value: Int64);
    procedure SetRowHeight(const Value: integer);

    function GetRowPaintState(const rowIdx : Int64) : TPaintRowState;


    procedure DoOnPaintRow(const itemRect : TRect; const index : Int64; const state : TPaintRowState);
    procedure Paint;override;
    procedure Resize; override;

    function GetRowFromY(const Y : integer) : integer;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure UpdateHoverRow(const X, Y : integer);


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
    property CurrentRow : Int64 read FCurrentRow write SetCurrentRow;
    property TopRow : Int64 read FTopRow;
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
    {$IF CompilerVersion >= 24.0}
      {$LEGACYIFEND ON}
    property StyleElements;
    {$IFEND}
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
  if (oldHoverRow <> -1) and RowInView(oldHoverRow) then
  begin
    rowState := GetRowPaintState(oldHoverRow);
    DoOnPaintRow(FRowRects[GetViewRow(oldHoverRow)], oldHoverRow, rowState);
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
  FCurrentRow := -1;
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
  result := true;
  if FRowCount = 0 then
    exit;

  if IsAtEnd then //nothing to do
    exit;

  scrollPos := Min(FScrollPos + 1, FRowCount - 1) ;
  ScrollBarScroll(Self,TScrollCode.scLineDown, scrollPos );
end;

function TVSoftVirtualListView.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  scrollPos : integer;
begin
  result := true;
  if FRowCount = 0 then
    exit;

  if IsAtTop then  //nothing to do
    exit;

  scrollPos := Max(0, FScrollPos - 1);
  ScrollBarScroll(Self,TScrollCode.scLineUp, scrollPos );
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

procedure TVSoftVirtualListView.DoRowChanged(const oldCurrentRow : integer);
var
  delta : integer;
  direction : TScrollDirection;
begin
  if FRowCount = 0 then
    exit;

  if not Assigned(FOnRowChangeEvent) then
    exit;

  delta := FCurrentRow - oldCurrentRow;
  if delta = 0 then
    exit
  else if delta < 0 then
    direction := TScrollDirection.sdUp
  else
    direction := TScrollDirection.sdDown;

  FOnRowChangeEvent(Self,FCurrentRow, direction, delta);
end;

procedure TVSoftVirtualListView.DoTrack(const newScrollPostition: integer);
var
  oldTopRow : integer;
begin
  oldTopRow := FTopRow;
  FTopRow := newScrollPostition;
  FScrollPos := FTopRow;
  if oldTopRow <> FTopRow then
  begin
    Invalidate;
    UpdateScrollBar;
  end;
end;

function TVSoftVirtualListView.GetRowFromY(const Y: integer): integer;
begin
  result := (Y div FRowHeight); //this is probably not quite right.
end;

function TVSoftVirtualListView.GetRowPaintState(const rowIdx: Int64): TPaintRowState;
begin
  if Self.Focused then
  begin
    result := rsFocusedNormal;
    if RowInView(rowIdx) then
    begin
      if (rowIdx = FCurrentRow) then
        result := rsFocusedSelected
      else if rowIdx = FHoverRow then
        result := rsFocusedHot;
    end;
  end
  else
  begin
    result := rsNormal;
    if RowInview(rowIdx) then
    begin
      if (rowIdx = FCurrentRow) then
        result := rsSelected
      else if rowIdx = FHoverRow then
        result := rsHot;
    end;
  end;
end;

function TVSoftVirtualListView.GetViewRow(const row: Int64): integer;
begin
  result := row - FTopRow;
  if result < 0 then
    result := 0;
  if result > FVisibleRows -1 then
    result := FVisibleRows -1;
end;

procedure TVSoftVirtualListView.InvalidateRow(const index: Int64);
var
  rowState : TPaintRowState;
begin
  //work out if the row is actually visible;
  if FRowCount = 0 then
    exit;
  if (index < 0) or (index > FRowCount -1) then
    exit;

  if (index >= FTopRow) and (index < FTopRow + FVisibleRows) then
  begin
    rowState := GetRowPaintState(index);
    DoOnPaintRow(FRowRects[GetViewRow(index)], index, rowState);
  end;
end;

function TVSoftVirtualListView.IsAtEnd: boolean;
begin
  result := FScrollPos = FRowCount - FSelectableRows;
end;

function TVSoftVirtualListView.IsAtTop: boolean;
begin
  result := FScrollPos = 0;
end;

procedure TVSoftVirtualListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_UP,VK_LEFT: DoLineUp(false);
    VK_DOWN, VK_RIGHT: DoLineDown(false);
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
  if FRowCount = 0 then
    exit;
  row := GetRowFromY(Y);
  if (row > FVisibleRows) or (row >= FRowCount) then
    exit;

  if FTopRow + row <> FCurrentRow then
  begin
    oldRow := FCurrentRow;
    FCurrentRow := FTopRow + row;
    DoRowChanged(oldRow);
    Invalidate;
    UpdateScrollBar;
  end;

end;

procedure TVSoftVirtualListView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  UpdateHoverRow(X, Y);
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
    if not DoubleBuffered and TStyleManager.IsCustomStyleActive {$IF CompilerVersion >= 24.0} and (seClient in StyleElements) {$IFEND} then
    begin
      Buffer := TBitmap.Create;
      Buffer.SetSize(ClientWidth, ClientHeight);
      LCanvas := Buffer.Canvas;
    end
    else
      LCanvas := Canvas;

    LStyle := StyleServices;

    if LStyle.Enabled {$IF CompilerVersion >= 24.0} and (seClient in StyleElements) {$IFEND} then
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
        rowState := GetRowPaintState(rowIdx);
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
    DoRowChanged(oldCurrentRow);

end;

function TVSoftVirtualListView.RowInView(const row: Int64): boolean;
begin
  result := (row >= FTopRow) and (row < (FTopRow + FSelectableRows));
end;

procedure TVSoftVirtualListView.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  case scrollCode of
    TScrollCode.scLineUp: DoLineUp(true);
    TScrollCode.scLineDown: DoLineDown(true);
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
    TScrollCode.scEndScroll: ;
  end;
end;

procedure TVSoftVirtualListView.DoLineDown(const fromScrollBar : boolean);
var
  oldCurrentRow : integer;
  rowState : TPaintRowState;
begin
  if FRowCount = 0 then
    exit;

  oldCurrentRow := FCurrentRow;

  //behavior depends on whether we are using the keyboard or the mouse on the scrollbar.
  //when we use the scrollbar, the current row doesn't change, we just scroll the view.

  if fromScrollBar then
  begin
    if (FTopRow + FSelectableRows -1) <  (FRowCount -1) then
      Inc(FTopRow)
    else
      exit;

    if FHoverRow <> -1 then
      Inc(FHoverRow);

    FScrollPos := FTopRow;
    //we scrolled so full paint.
    Invalidate;
    UpdateScrollBar;
  end
  else //from keyboard
  begin
    if RowInView(FCurrentRow) then
    begin
      //if the currentRow is visible, then we can try to move the current row if it's not at the bottom.
      if (FCurrentRow - FTopRow < FSelectableRows - 1) then
      begin
        Inc(FCurrentRow);
        //no scrolling required so just paint the affected rows.
        //there may not have been a current row before.
        if (oldCurrentRow >= 0) and RowInView(oldCurrentRow) then
        begin
          rowState := GetRowPaintState(oldCurrentRow);
          DoOnPaintRow(FRowRects[GetViewRow(oldCurrentRow)], oldCurrentRow , rowState);
        end;
        rowState := GetRowPaintState(FCurrentRow);
        DoOnPaintRow(FRowRects[GetViewRow(FCurrentRow)], FCurrentRow , rowState);
        DoRowChanged(oldCurrentRow);
        FScrollPos := FTopRow;
        UpdateScrollBar;
      end
      else
      begin
        //Current Row isn't in the view, so we will need a full paint
        if FCurrentRow < FRowCount -1 then
        begin
          Inc(FCurrentRow);
          Inc(FTopRow);
          if FHoverRow <> -1 then
            Inc(FHoverRow);
          FScrollPos := FTopRow;
          DoRowChanged(oldCurrentRow);
          Invalidate;
          UpdateScrollBar;
        end;
      end;
    end
    else
    begin
      if FCurrentRow < FRowCount -1 then
      begin
        Inc(FCurrentRow);
        FTopRow := FCurrentRow;
        FScrollPos := FTopRow;
        DoRowChanged(oldCurrentRow);
        Invalidate;
        UpdateScrollBar;
      end;
    end;
  end;
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
  if FRowCount = 0 then
    exit;

  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  fullRepaint := false;
  delta := 0;

  if fromScrollBar  then
  begin
    //we do not change current row here.
    FTopRow := newScrollPostition;
    if FTopRow > (FRowCount - FSelectableRows) then
      FTopRow := FRowCount - FSelectableRows;
    if oldTopRow <> FTopRow then
    begin
      delta := FTopRow - oldTopRow;
      fullRepaint := true;
    end
  end
  else
  begin //from keyboard
    pageSize := Min(FSelectableRows, FRowCount -1);

    if RowInView(FCurrentRow) and ((FCurrentRow + pageSize) <= (FTopRow +  FSelectableRows)) then
    begin
      FCurrentRow := FTopRow + FSelectableRows -1;
    end
    else
    begin
      Inc(FCurrentRow, pageSize);
      FCurrentRow := Min(FCurrentRow, FRowCount -1);
      //position current row at the bottom
      FTopRow := Max(0,FCurrentRow - FSelectableRows + 1);
      fullRepaint := true;
    end;
    delta := FCurrentRow - oldCurrentRow;
  end;
  if delta > 0 then
  begin
    if not fullRepaint then
    begin
      rowState := GetRowPaintState(oldCurrentRow);
      DoOnPaintRow(FRowRects[GetViewRow(oldCurrentRow)], oldCurrentRow, rowState);
      rowState := GetRowPaintState(FCurrentRow);
      DoOnPaintRow(FRowRects[GetViewRow(FCurrentRow)], FCurrentRow , rowState);
    end
    else
      Invalidate;

    DoRowChanged(oldCurrentRow);
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
  if FRowCount = 0 then
    exit;

  oldTopRow := FTopRow;
  oldCurrentRow := FCurrentRow;
  FCurrentRow := FRowCount - 1;
  FTopRow := FRowCount - FSelectableRows;
  FScrollPos := FRowCount -1;

  if FTopRow <> oldTopRow then
    //need a full repaint.
    Invalidate
  else if FCurrentRow <> oldCurrentRow then
  begin
    rowState := GetRowPaintState(oldCurrentRow);
    DoOnPaintRow(FRowRects[GetViewRow(oldCurrentRow)], oldCurrentRow, rowState);
    rowState := GetRowPaintState(FCurrentRow);
    DoOnPaintRow(FRowRects[GetViewRow(FCurrentRow)], FCurrentRow , rowState);
  end;
  DoRowChanged(oldCurrentRow);
  UpdateScrollBar;
end;

procedure TVSoftVirtualListView.DoGoTop;
var
  oldCurrentRow : integer;
  oldTopRow : integer;
  rowState : TPaintRowState;
begin
  if FRowCount = 0 then
    exit;

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
    FScrollPos := 0;
    UpdateScrollBar;
    DoRowChanged(oldCurrentRow);
  end;
end;

procedure TVSoftVirtualListView.DoLineUp(const fromScrollBar : boolean);
var
  oldCurrentRow : integer;
  rowState : TPaintRowState;
begin
  if FRowCount = 0 then
    exit;

  oldCurrentRow := FCurrentRow;

  if fromScrollBar then
  begin
    if FTopRow > 0 then
    begin
      Dec(FTopRow);
      if FHoverRow > 0 then
        Dec(FHoverRow);

      FScrollPos := FTopRow;
      //we scrolled so full paint.
      Invalidate;
      UpdateScrollBar;
    end;
  end
  else
  begin
    if RowInView(FCurrentRow) then
    begin
      //if the currentRow is visible, then we can try to move the current row if it's not at the bottom.
      if  ((FCurrentRow - FTopRow ) > 0) then
      begin
        Dec(FCurrentRow);
        if FHoverRow > 0 then
          Dec(FHoverRow);
        //no scrolling required so just paint the affected rows.
        //there may not have been a current row before.
        if (oldCurrentRow >= 0) and RowInView(oldCurrentRow) then
        begin
          rowState := GetRowPaintState(oldCurrentRow);
          DoOnPaintRow(FRowRects[GetViewRow(oldCurrentRow)], oldCurrentRow , rowState);
        end;
        rowState := GetRowPaintState(FCurrentRow);
        DoOnPaintRow(FRowRects[GetViewRow(FCurrentRow)], FCurrentRow , rowState);
        DoRowChanged(oldCurrentRow);
        FScrollPos := FTopRow;
        UpdateScrollBar;
      end
      else
      begin
        //Current Row isn't in the view, so we will need a full paint
        if (FCurrentRow > 0) and (FTopRow > 0) then
        begin
          Dec(FCurrentRow);
          Dec(FTopRow);
          if FHoverRow > 0 then
            Dec(FHoverRow);
          FScrollPos := FTopRow;
          DoRowChanged(oldCurrentRow);
          Invalidate;
          UpdateScrollBar;
        end;
      end;
    end
    else
    begin
      if FCurrentRow > 0 then
      begin
        Dec(FCurrentRow);
        if FCurrentRow < FTopRow then
          FTopRow := FCurrentRow
        else
          FTopRow := Max(FCurrentRow - FSelectableRows - 1, 0);
        FScrollPos := FTopRow;
        DoRowChanged(oldCurrentRow);
        Invalidate;
        UpdateScrollBar;
      end;
    end;

  end;
//
//  if FTopRow <> oldTopRow then
//    Invalidate
//  else
//  begin
//    rowState := GetRowPaintState(oldCurrentRow);
//    DoOnPaintRow(FRowRects[oldCurrentRow], oldTopRow + oldCurrentRow, rowState);
//    rowState := GetRowPaintState(FCurrentRow);
//    DoOnPaintRow(FRowRects[FCurrentRow], FTopRow + FCurrentRow , rowState);
//  end;
//  FScrollPos := FTopRow;
//  UpdateScrollBar;
//  DoRowChanged(oldCurrentRow);
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
  if FRowCount = 0 then
    exit;


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
    //from keyboard
    pageSize := Min(FSelectableRows, FRowCount -1);
    if RowInView(FCurrentRow) and (FCurrentRow > FTopRow)  then
    begin
      FCurrentRow := FTopRow;
    end
    else
    begin
      Dec(FTopRow, pageSize);
      FTopRow := Max(FTopRow, 0);
      FCurrentRow := FTopRow;
      fullRepaint := true;
    end;
    delta := FCurrentRow  - oldCurrentRow;
  end;


  if delta < 0 then
  begin
    if not fullRepaint then
    begin
      rowState := GetRowPaintState(oldCurrentRow);
      DoOnPaintRow(FRowRects[GetViewRow(oldCurrentRow)], oldCurrentRow, rowState);
      rowState := GetRowPaintState(FCurrentRow);
      DoOnPaintRow(FRowRects[GetViewRow(FCurrentRow)], FCurrentRow , rowState);
    end
    else
      Invalidate;

    DoRowChanged(oldCurrentRow);
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

procedure TVSoftVirtualListView.SetCurrentRow(const Value: Int64);
var
  oldRow : Int64;
begin
  if (FCurrentRow <> Value) and (Value < FRowCount) then
  begin
    oldRow := FCurrentRow;
    FCurrentRow := Value;
    DoRowChanged(oldRow);
    Invalidate; //do full repaint
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
    if FRowCount = 0 then
    begin
      FCurrentRow := -1;
      FHoverRow := -1;
    end;

    if HandleAllocated then
    begin
      UpdateVisibleRows;
      Invalidate;
    end;
  end;
end;

procedure TVSoftVirtualListView.UpdateHoverRow(const X, Y: integer);
var
  row : Integer;
  oldHoverRow : integer;
  rowState : TPaintRowState;
begin
  row := FTopRow + GetRowFromY(Y);
  if row <> FHoverRow then
  begin
    oldHoverRow := FHoverRow;
    FHoverRow := row;
    if (oldHoverRow <> -1) and RowInView(oldHoverRow) then
    begin
      rowState := GetRowPaintState(oldHoverRow);
      DoOnPaintRow(FRowRects[GetViewRow(oldHoverRow)],oldHoverRow, rowState);
    end;
    if (FHoverRow > -1) and RowInView(FHoverRow)  then
    begin
      rowState := GetRowPaintState(FHoverRow);
      DoOnPaintRow(FRowRects[GetViewRow(FHoverRow)], FHoverRow , rowState);
    end
  end;
end;

procedure TVSoftVirtualListView.UpdateScrollBar;
var
  sbInfo : TScrollInfo;
  //prevVisible : boolean;
begin
  if not HandleAllocated then
    exit;

//  prevVisible := FScrollBarVisible;
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
  //doesn't appear to be needed.. will leave in case styles affect this.
//  if prevVisible <> FScrollBarVisible then
//    Invalidate;
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
    FSelectableRows := Min(FVisibleRows, FRowCount); //the number of full rows
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
  Message.Result := 0;
  with Message do
  begin
    if ScrollCode = 8 then
      exit;
    p := pos;
    Self.ScrollBarScroll(Self, TScrollCode(ScrollCode), p);
  end;
end;

end.
