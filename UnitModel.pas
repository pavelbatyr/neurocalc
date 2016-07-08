unit UnitModel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ToolWin, Vcl.ComCtrls, System.Actions, Vcl.ActnList, Vcl.Menus,
  System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.StdCtrls,
  System.Math, System.Types, UnitNeurons;

type
  TButtonEventHandlers = class
    procedure btnCreateInpNeuronClick(Sender: TObject);
    procedure btnCreateNeuronClick(Sender: TObject);
    procedure btnCreateOutNeuronClick(Sender: TObject);
    procedure btnCreateBiasNeuronClick(Sender: TObject);
    procedure btnCreateLinkClick(Sender: TObject);
    procedure btnRemoveNeuronClick(Sender: TObject);
  end;

  TPaintBoxEventHandlers = class
    procedure PBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure PBoxMouseMove(Sender: TObject; Shift: TShiftState;
      x, y: Integer);
    procedure PBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure PBoxPaint(Sender: TObject);
  end;

procedure modelSetUp();

var
  PBox: TPaintBox;
  PBleft, PBtop, PBwidth, PBheight: Integer;
  neuronSize, inpNeuronSize: Integer;
  zoomedNeuronSize, zoomedInpNeuronSize: Integer;
  xdif, ydif: Integer;
  isDown: boolean;
  currentIndex: Integer;

  //right click coords
  x0, y0: Integer;
  setConnectionMode, firstConIsSet: boolean;

  //some PaintBox object selected
  selected: boolean;
  rightMouseClicked: Boolean;
  forRemoval: boolean;
  zoom, oldZoom: Double;
  // axes offsets used for centering when visual area is being scaled
  offSetX, offSetY: Double;
  // difference between neurons[i].x and neurons[i].visX
  shiftX, shiftY: Integer;
  zoomedX, zoomedY: Double;
  btnCreateInpNeuron, btnCreateNeuron, btnCreateOutNeuron,
    btnCreateBiasNeuron, btnCreateLink, btnRemoveNeuron: TButton;
  buttonsHandlers: TButtonEventHandlers;
  paintboxHandlers: TPaintBoxEventHandlers;

const
  // generating buttons that create neurons
  neuronBtnsLeftPos = 20;
  posFirstBtnTop = 65;
  btnsMargin = 10;
  neuronBtnsWidth = 85;
  btnInpNeuronHeight = 55;
  btnNeuronHeight = 40;
  btnOutNeuronHeight = 55;
  btnBiasNeuronHeight = 55;
  btnLinkHeight = 40;
  btnRemoveNeuronHeight = 40;

implementation

uses
  MForm;

procedure createAndsetUpNeuron();
begin
  neurons[neuronUnitsCount] := TNeuron.Create();
  neurons[neuronUnitsCount].x := random(PBwidth - zoomedNeuronSize);
  neurons[neuronUnitsCount].y := random(PBheight - zoomedNeuronSize);
  neurons[neuronUnitsCount].visX := neurons[neuronUnitsCount].x;
  neurons[neuronUnitsCount].visY := neurons[neuronUnitsCount].y;
  neurons[neuronUnitsCount].index := neuronUnitsCount;
  neurons[neuronUnitsCount].hasConnections := false;
  neurons[neuronUnitsCount].neuronType := 1;
  neurons[neuronUnitsCount].isCalculated := false;
  inc(neuronsCount);
  inc(neuronUnitsCount);
end;

procedure createAndsetUpBiasNeuron();
begin
  neurons[neuronUnitsCount] := TBiasNeuron.Create();
  neurons[neuronUnitsCount].x := random(PBwidth - zoomedNeuronSize);
  neurons[neuronUnitsCount].y := random(PBheight - zoomedNeuronSize);
  neurons[neuronUnitsCount].visX := neurons[neuronUnitsCount].x;
  neurons[neuronUnitsCount].visY := neurons[neuronUnitsCount].y;
  neurons[neuronUnitsCount].index := neuronUnitsCount;
  neurons[neuronUnitsCount].hasConnections := false;
  neurons[neuronUnitsCount].neuronType := 2;
  inc(biasNeuronsCount);
  inc(neuronUnitsCount);
end;

procedure createAndsetUpOutNeuron();
begin
  neurons[neuronUnitsCount] := TOutNeuron.Create();
  neurons[neuronUnitsCount].x := random(PBwidth - zoomedNeuronSize);
  neurons[neuronUnitsCount].y := random(PBheight - zoomedNeuronSize);
  neurons[neuronUnitsCount].visX := neurons[neuronUnitsCount].x;
  neurons[neuronUnitsCount].visY := neurons[neuronUnitsCount].y;
  neurons[neuronUnitsCount].index := neuronUnitsCount;
  neurons[neuronUnitsCount].hasConnections := false;
  neurons[neuronUnitsCount].neuronType := 3;
  neurons[neuronUnitsCount].isCalculated := false;
  inc(outNeuronsCount);
  inc(neuronUnitsCount);
end;

procedure createAndsetUpInpNeuron();
begin
  neurons[neuronUnitsCount] := TInpNeuron.Create();
  neurons[neuronUnitsCount].x := random(PBwidth -
    zoomedInpNeuronSize);
  neurons[neuronUnitsCount].y := random(PBheight -
    zoomedInpNeuronSize);
  neurons[neuronUnitsCount].visX := neurons[neuronUnitsCount].x;
  neurons[neuronUnitsCount].visY := neurons[neuronUnitsCount].y;
  neurons[neuronUnitsCount].index := neuronUnitsCount;
  neurons[neuronUnitsCount].hasConnections := false;
  neurons[neuronUnitsCount].neuronType := 0;
  inc(inpNeuronsCount);
  inc(neuronUnitsCount);
end;

procedure resetCreatingConnection();
begin
// failure of creating a connection
  setConnectionMode := false;
  firstConIsSet := false;
end;

procedure TButtonEventHandlers.btnCreateInpNeuronClick;
begin
  resetCreatingConnection();
  createAndSetUpInpNeuron();
  PBox.Repaint;
end;

procedure TButtonEventHandlers.btnCreateNeuronClick;
begin
  resetCreatingConnection();
  createAndSetUpNeuron();
  PBox.Repaint;
end;

procedure TButtonEventHandlers.btnCreateOutNeuronClick;
begin
  resetCreatingConnection();
  createAndSetUpOutNeuron();
  PBox.Repaint;
end;

procedure TButtonEventHandlers.btnCreateBiasNeuronClick;
begin
  resetCreatingConnection();
  createAndSetUpBiasNeuron();
  PBox.Repaint;
end;

procedure TButtonEventHandlers.btnCreateLinkClick(Sender: TObject);
begin
  firstConIsSet := false;
  setConnectionMode := true;
end;

procedure TButtonEventHandlers.btnRemoveNeuronClick(Sender: TObject);
begin
  forRemoval := true;
end;

procedure modelSetUp();
begin

  PBleft := 150;
  PBtop := 50;
  PBwidth := FormMain.Width - 200;
  PBheight := FormMain.Height - 200;

  neuronSize := 25;
  zoomedNeuronSize := neuronSize;
  inpNeuronSize := neuronSize - 10;
  zoomedInpNeuronSize := inpNeuronSize;
  zoom := 1.0;
  offSetX := 0.0;
  offSetY := 0.0;
  shiftX := 0;
  shiftY := 0;

  isDown := false;
  setConnectionMode := false;
  firstConIsSet := false;
  selected := false;
  rightMouseClicked := false;
  forRemoval := False;

  paintboxHandlers := TPaintBoxEventHandlers.Create;
  PBox := TPaintBox.Create(FormMain);
  with PBox do
  begin
    Parent := FormMain.MainPanel;
    Color := clWhite;
    Left := PBleft;
    Top := PBtop;
    Height := PBheight;
    Width := PBwidth;

    onPaint := paintboxHandlers.PBoxPaint;
    onMouseDown := paintboxHandlers.PBoxMouseDown;
    onMouseMove := paintboxHandlers.PBoxMouseMove;
    onMouseUp := paintboxHandlers.PBoxMouseUp;
  end;

  buttonsHandlers := TButtonEventHandlers.Create;
  btnCreateInpNeuron := TButton.Create(FormMain);
  with btnCreateInpNeuron do
  begin
    Parent := FormMain.MainPanel;
    Left := neuronBtnsLeftPos;
    Top := posFirstBtnTop;
    Height := btnInpNeuronHeight;
    Width := neuronBtnsWidth;
    Caption := 'создать входной нейрон';
    WordWrap := true;
    OnClick := buttonsHandlers.btnCreateInpNeuronClick;
  end;

  btnCreateNeuron := TButton.Create(FormMain);
  with btnCreateNeuron do
  begin
    Parent := FormMain.MainPanel;
    Left := neuronBtnsLeftPos;
    Top := posFirstBtnTop + btnInpNeuronHeight + btnsMargin;
    Height := btnNeuronHeight;
    Width := neuronBtnsWidth;
    Caption := 'создать нейрон';
    WordWrap := true;
    OnClick := buttonsHandlers.btnCreateNeuronClick;
  end;

  btnCreateOutNeuron := TButton.Create(FormMain);
  with btnCreateOutNeuron do
  begin
    Parent := FormMain.MainPanel;
    Left := neuronBtnsLeftPos;
    Top := btnCreateNeuron.Top + btnNeuronHeight + btnsMargin;
    Height := btnOutNeuronHeight;
    Width := neuronBtnsWidth;
    Caption := 'создать выходной нейрон';
    WordWrap := true;
    OnClick := buttonsHandlers.btnCreateOutNeuronClick;
  end;

  btnCreateBiasNeuron := TButton.Create(FormMain);
  with btnCreateBiasNeuron do
  begin
    Parent := FormMain.MainPanel;
    Left := neuronBtnsLeftPos;
    Top := btnCreateOutNeuron.Top + btnOutNeuronHeight + btnsMargin;
    Height := btnBiasNeuronHeight;
    Width := neuronBtnsWidth;
    Caption := 'создать нейрон смещения';
    WordWrap := True;
    OnClick := buttonsHandlers.btnCreateBiasNeuronClick;
  end;

  btnCreateLink := TButton.Create(FormMain);
  with btnCreateLink do
  begin
    Parent := FormMain.MainPanel;
    Left := neuronBtnsLeftPos;
    Top := btnCreateBiasNeuron.Top + btnBiasNeuronHeight + btnsMargin;
    Height := btnLinkHeight;
    Width := neuronBtnsWidth;
    Caption := 'создать' + sLineBreak + 'связь';
    WordWrap := True;
    OnClick := buttonsHandlers.btnCreateLinkClick;
  end;

  btnRemoveNeuron := TButton.Create(FormMain);
  with btnRemoveNeuron do
  begin
    Parent := FormMain.MainPanel;
    Left := neuronBtnsLeftPos;
    Top := btnCreateLink.Top + btnLinkHeight + btnsMargin;
    Height := btnRemoveNeuronHeight;
    Width := neuronBtnsWidth;
    Caption := 'удалить' + sLineBreak + 'нейрон';
    WordWrap := True;
    OnClick := buttonsHandlers.btnRemoveNeuronClick;
  end;
end;

procedure removeNeuron(x, y: Integer);
var
  i, j, k, l: Integer;
begin
  i := 0;
  while (i < neuronUnitsCount) do
  begin
    if (x >= neurons[i].visx) and (x <= neurons[i].visx +
      zoomedneuronSize) and (y >= neurons[i].visy) and (y <=
      neurons[i].visy + zoomedneuronSize) then
    begin
      k := 0;
      while (k < conCount) do
      begin
          // if vertex removed is a beginning or an ending of a connection
        if (connections[k].n1 = i) or (connections[k].n2 = i) then
        begin
          for l := k to conCount - 2 do
            connections[l] := connections[l + 1];
          Dec(conCount);
          Dec(k);
        end;
        Inc(k);
      end;
      neurons[i].free;
      for j := i to neuronUnitsCount - 2 do
      begin
        neurons[j] := neurons[j + 1];
        for k := 0 to conCount - 1 do
        begin
            // If a connection exists that contains the removed vertex,
            // then update numbers of vertices of this connection.
          if (connections[k].n1 = j + 1) then
            Dec(connections[k].n1);
          if (connections[k].n2 = j + 1) then
            Dec(connections[k].n2);
        end;
      end;
      Dec(neuronUnitsCount);
      forRemoval := false;
    end;
    Inc(i);
  end;
end;

procedure prepareNeuronForMovement(x, y: Integer);
var
  i: Integer;
begin
  isDown := true;
  selected := false;
  currentIndex := -1; // if there's no neuron in the clicked area
  for i := 0 to neuronUnitsCount - 1 do
  begin
    if (x >= neurons[i].visx) and (x <= neurons[i].visx +
      zoomedInpNeuronSize) and (y >= neurons[i].visy) and (y <=
      neurons[i].visy + zoomedInpNeuronSize) and (neurons[i] is
      TInpNeuron) then
    begin
    // record number of this neuron from the neurons list
      currentIndex := i;

    // some neuron was selected
      selected := true;
    // axes offsets from the upper left corner of neuron
      xdif := x - neurons[i].x;
      ydif := y - neurons[i].y;
    end
    else if (x >= neurons[i].visx) and (x <= neurons[i].visx +
      zoomedNeuronSize) and (y >= neurons[i].visy) and (y <=
      neurons[i].visy + zoomedNeuronSize) and ((neurons[i] is
      TNeuron) or (neurons[i] is TOutNeuron) or (neurons[i] is
      TBiasNeuron)) then
    begin
    // record number of this neuron from the neurons list
      currentIndex := i;

    // some neuron was selected
      selected := true;

    // axes offsets from the upper left corner of neuron
      xdif := x - neurons[i].x;
      ydif := y - neurons[i].y;
    end;
  end;
  if (rightMouseClicked = false) and (currentIndex <> -1) then
  begin
    if neurons[0] <> nil then
    begin
      shiftX := neurons[currentIndex].visX - neurons[currentIndex].x;
      shiftY := neurons[currentIndex].visY - neurons[currentIndex].y;
    end;
    zoomedX := (neurons[currentIndex].x + shiftX) * zoom - (neurons
      [currentIndex].x + shiftX);
    zoomedY := (neurons[currentIndex].y + shiftY) * zoom - (neurons
      [currentIndex].y + shiftY);
  end;
end;

procedure TPaintBoxEventHandlers.PBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; x, y: Integer);
var
  i: Integer;
begin
  if forRemoval = false then
  begin
    if Button = mbRight then
    begin
      rightMouseClicked := true;
      x0 := x;
      y0 := y;
      for i := 0 to neuronUnitsCount - 1 do
      begin
        neurons[i].startX := neurons[i].visX;
        neurons[i].startY := neurons[i].visY;
      end;
    end
    else
      rightMouseClicked := false;
    prepareNeuronForMovement(x, y);
  end
  else
  begin
    removeNeuron(x, y);
    PBox.Refresh;
  end;
end;

// Moving selected neuron or moving visual area at right click
procedure TPaintBoxEventHandlers.PBoxMouseMove(Sender: TObject;
  Shift: TShiftState; x, y: Integer);
var
  rectUpdated: TRect;
  HWND: THandle;
  i: Integer;
const
  neuronRectMargin = 25;
  inpneuronRectMargin = 35;
begin
// If after a neuron was selected, PaintBox area is left-clicked
// For input neurons (first layer) only
  if (setConnectionMode = false) and (currentIndex <> -1) and (isDown
    = true) and (x < PBwidth - zoomedInpNeuronSize) and (x > 0)
    and (y < PBheight - zoomedInpNeuronSize) and (y > 0) and (neurons
    [currentIndex] is TInpNeuron) then
  begin
    neurons[currentIndex].x := x - xdif;
    neurons[currentIndex].y := y - ydif;
    neurons[currentIndex].visX := Round(neurons[currentIndex].x +
      shiftX + zoomedX + offsetX);
    neurons[currentIndex].visY := Round(neurons[currentIndex].y +
      shiftY + zoomedY + offsetY);
    rectUpdated.Left := x - xdif - inpneuronRectMargin + PBleft;
    rectUpdated.Top := y - ydif - inpneuronRectMargin + PBtop;
    rectUpdated.Right := x - xdif + zoomedInpNeuronSize +
      inpneuronRectMargin + PBleft;
    rectUpdated.Bottom := y - ydif + zoomedInpNeuronSize +
      inpneuronRectMargin + PBtop;
    HWND := PBox.Parent.Handle;
    if neurons[currentIndex].hasConnections = false then
      invalidateRect(HWND, @rectUpdated, false)
    else
    begin
      rectUpdated.Left := PBleft + 5;
      rectUpdated.Top := PBtop + 5;
      rectUpdated.Right := PBwidth + PBleft - 5;
      rectUpdated.Bottom := PBheight + PBtop - 5;
      InvalidateRect(HWND, @rectUpdated, False);
    end;
  end
  else
// If after a neuron was selected, PaintBox area is left-clicked
// For any neurons except input neurons
 if (setConnectionMode = false) and (currentIndex <> -1) and (isDown
   = true) and (x < PBwidth - zoomedNeuronSize) and (x > 0) and (y
   < PBheight - zoomedNeuronSize) and (y > 0) then
  begin
    neurons[currentIndex].x := x - xdif;
    neurons[currentIndex].y := y - ydif;
    neurons[currentIndex].visX := Round(neurons[currentIndex].x +
      shiftX + zoomedX + offsetX);
    neurons[currentIndex].visY := Round(neurons[currentIndex].y +
      shiftY + zoomedY + offsetY);
    rectUpdated.Left := x - xdif - neuronRectMargin + PBleft;
    rectUpdated.Top := y - ydif - neuronRectMargin + PBtop;
    rectUpdated.Right := x - xdif + zoomedNeuronSize +
      neuronRectMargin + PBleft;
    rectUpdated.Bottom := y - ydif + zoomedNeuronSize +
      neuronRectMargin + PBtop;
    HWND := PBox.Parent.Handle;
    if neurons[currentIndex].hasConnections = false then
      invalidateRect(HWND, @rectUpdated, false)
    else
    begin
      rectUpdated.Left := PBleft + 5;
      rectUpdated.Top := PBtop + 5;
      rectUpdated.Right := PBwidth + PBleft - 5;
      rectUpdated.Bottom := PBheight + PBtop - 5;
      InvalidateRect(HWND, @rectUpdated, False);
    end;
  end;

  // Moving visual area on right click
  if (selected = False) and (rightMouseClicked = true) then
  begin
    for i := 0 to neuronUnitsCount - 1 do
    begin
    // startX - initial value of visX
    // x - x0 - offset from the start position
      neurons[i].visX := neurons[i].startX + x - x0;
      neurons[i].visY := neurons[i].startY + y - y0;
    end;
    // required for centering when scaling visual area
    if neurons[0] <> nil then
    begin
      shiftX := neurons[0].visX - neurons[0].x;
      shiftY := neurons[0].visY - neurons[0].y;
    end;
    PBox.Refresh;
  end;

end;

procedure TPaintBoxEventHandlers.PBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; x, y: Integer);
var
  i: Integer;
begin
  isDown := false;
  rightMouseClicked := False;
  for i := 0 to neuronUnitsCount - 1 do
  begin
    if (x >= neurons[i].visx) and (x <= neurons[i].visx +
      zoomedNeuronSize) and (y >= neurons[i].visy) and (y <=
      neurons[i].visy + zoomedNeuronSize) then
    begin
      if setConnectionMode = true then
      begin
        if (firstConIsSet = false) and not (neurons[i] is
          TOutNeuron) then
        // establish a connection from the first neuron
        begin
          connections[conCount].n1 := currentIndex;
          firstConIsSet := true;
        end
        else if (firstConIsSet = true) and ((neurons[i] is
          TNeuron) or (neurons[i] is TOutNeuron)) then
        // establish a connection to the second neuron
        begin
          connections[conCount].n2 := i;

          neurons[connections[conCount].n1].hasConnections := true;
          neurons[i].hasConnections := true;

          inc(conCount);
          firstConIsSet := false;
          setConnectionMode := false;
        end
        else
          setConnectionMode := false;
      end;
    end;
  end;
  PBox.Refresh;
end;

// Calculating coords of arrows of connections
procedure drawArrow(var endLeftX, endLeftY, endRightX, endRightY:
  Integer; x1, y1, x2, y2: Integer; var arrowEndX, arrowEndY:
  Integer; neuronSize: Integer);
var
  headLength: Integer;
  xbase: Integer;
  xLineDelta, xLineDelta2: Integer;
  xLineUnitDelta: Double;
  xNormalDelta: Integer;
  xNormalUnitDelta: Double;
  ybase: Integer;
  yLineDelta, yLineDelta2: Integer;
  yLineUnitDelta: Double;
  yNormalDelta: Integer;
  yNormalUnitDelta: Double;
begin
  xLineDelta := x2 - x1;
  yLineDelta := y2 - y1;
  if (abs(xLineDelta) > neuronSize) or (abs(yLineDelta) >
    neuronSize) then
  begin
  // cosine
    xLineUnitDelta := xLineDelta / SQRT(SQR(xLineDelta) + SQR(yLineDelta));
  // sine
    yLineUnitDelta := yLineDelta / SQRT(SQR(xLineDelta) + SQR(yLineDelta));

  // (xBase,yBase) - arrow basement coords
    headLength := Round(4 * zoom);
    xbase := x2 - ROUND(headLength * xLineUnitDelta) - ROUND(neuronSize
      div 2 * xLineUnitDelta);
    ybase := y2 - ROUND(headLength * yLineUnitDelta) - ROUND(neuronSize
      div 2 * yLineUnitDelta);

    xLineDelta2 := x2 - x1 - ROUND(neuronSize div 2 * xLineUnitDelta);
    yLineDelta2 := y2 - y1 - ROUND(neuronSize div 2 * yLineUnitDelta);

    arrowEndX := x2 - ROUND(neuronSize div 2 * xLineUnitDelta);
    arrowEndY := y2 - ROUND(neuronSize div 2 * yLineUnitDelta);
    xNormalDelta := yLineDelta2;
    yNormalDelta := -xLineDelta2;
    xNormalUnitDelta := xNormalDelta / SQRT(SQR(xNormalDelta) +
      SQR(yNormalDelta));
    yNormalUnitDelta := yNormalDelta / SQRT(SQR(xNormalDelta) +
      SQR(yNormalDelta));
    endLeftX := xbase + ROUND(headLength * xNormalUnitDelta) -
      ROUND(neuronSize div 2 * xLineUnitDelta);
    ;
    endLeftY := ybase + ROUND(headLength * yNormalUnitDelta) -
      ROUND(neuronSize div 2 * yLineUnitDelta);
    ;
    endRightX := xbase - ROUND(headLength * xNormalUnitDelta) -
      ROUND(neuronSize div 2 * xLineUnitDelta);
    ;
    endRightY := ybase - ROUND(headLength * yNormalUnitDelta) -
      ROUND(neuronSize div 2 * yLineUnitDelta);
    ;
  end;
end;

procedure TPaintBoxEventHandlers.PBoxPaint(Sender: TObject);
var
  i, j: Integer;
  arrowLeftX, arrowLeftY, arrowRightX, arrowRightY: Integer;
  x1, x2, y1, y2, arrowEndX, arrowEndY: Integer;
begin

// painting outer frame of the PaintBox
  PBox.Canvas.Pen.Color := RGB(0, 0, 0);
  PBox.Canvas.Rectangle(0, 0, PBWidth, PBheight);


// painting connections
  for j := 0 to conCount - 1 do
  begin
    PBox.Canvas.Pen.Color := RGB(48, 48, 54);
    if (neurons[connections[j].n1] is TNeuron) or (neurons[connections
      [j].n1] is TOutNeuron) then
    begin
      x1 := neurons[connections[j].n1].visx + zoomedNeuronSize div 2;
      y1 := neurons[connections[j].n1].visy + zoomedNeuronSize div 2;
      x2 := neurons[connections[j].n2].visx + zoomedNeuronSize div 2;
      y2 := neurons[connections[j].n2].visy + zoomedNeuronSize div 2;
      PBox.Canvas.MoveTo(x1, y1);
      PBox.Canvas.LineTo(x2, y2);
    end
    else if (neurons[connections[j].n1] is TBiasNeuron) then
    begin
      PBox.Canvas.Pen.Color := RGB(140, 140, 140);
      x1 := neurons[connections[j].n1].visx + zoomedNeuronSize div 2;
      y1 := neurons[connections[j].n1].visy + zoomedNeuronSize div 2;
      x2 := neurons[connections[j].n2].visx + zoomedNeuronSize div 2;
      y2 := neurons[connections[j].n2].visy + zoomedNeuronSize div 2;
      PBox.Canvas.MoveTo(x1, y1);
      PBox.Canvas.LineTo(x2, y2);
    end
    else // neurons[j] is input neuron
    begin
      x1 := neurons[connections[j].n1].visx + zoomedInpNeuronSize
        div 2;
      y1 := neurons[connections[j].n1].visy + zoomedInpNeuronSize
        div 2;
      x2 := neurons[connections[j].n2].visx + zoomedNeuronSize div 2;
      y2 := neurons[connections[j].n2].visy + zoomedNeuronSize div 2;
      PBox.Canvas.MoveTo(x1, y1);
      PBox.Canvas.LineTo(x2, y2);
    end;

  // painting arrows
    PBox.Canvas.Brush.Color := RGB(48, 48, 54);
    PBox.Canvas.Pen.Color := RGB(48, 48, 54);
    if (neurons[connections[j].n1] is TBiasNeuron) then
    begin
      PBox.Canvas.Brush.Color := RGB(140, 140, 140);
      PBox.Canvas.Pen.Color := RGB(140, 140, 140);
    end;
    drawArrow(arrowLeftX, arrowLeftY, arrowRightX, arrowRightY,
      x1, y1, x2, y2, arrowEndX, arrowEndY, zoomedNeuronSize);
    PBox.Canvas.Polygon([Point(arrowEndX, arrowEndY), Point(arrowLeftX,
      arrowLeftY), Point(arrowRightX, arrowRightY)]);

  end;

// painting all kinds of neurons
  PBox.Canvas.Pen.Color := RGB(0, 0, 0);
  for i := 0 to neuronUnitsCount - 1 do
  begin

    if neurons[i] is TNeuron then
    begin
      PBox.Canvas.Brush.Color := RGB(51, 153, 204);
      PBox.Canvas.Ellipse(neurons[i].visx, neurons[i].visy,
        neurons[i].visx + zoomedNeuronSize, neurons[i].visy +
        zoomedNeuronSize);
    end
    else if neurons[i] is TInpNeuron then
    begin
      PBox.Canvas.Brush.Color := RGB(152, 251, 152);
      PBox.Canvas.Rectangle(neurons[i].visx, neurons[i].visy,
        neurons[i].visx + zoomedInpNeuronSize, neurons[i].visy +
        zoomedInpNeuronSize);
    end
    else if neurons[i] is TOutNeuron then
    begin
      PBox.Canvas.Brush.Color := RGB(240, 245, 107);
      PBox.Canvas.Ellipse(neurons[i].visx, neurons[i].visy,
        neurons[i].visx + zoomedNeuronSize, neurons[i].visy +
        zoomedNeuronSize);
    end
    else if neurons[i] is TBiasNeuron then
    begin
      PBox.Canvas.Pen.Color := RGB(140, 140, 140);
      PBox.Canvas.Brush.Color := RGB(210, 210, 210);
      PBox.Canvas.Ellipse(neurons[i].visx, neurons[i].visy,
        neurons[i].visx + zoomedNeuronSize, neurons[i].visy +
        zoomedNeuronSize);
      PBox.Canvas.Pen.Color := RGB(0, 0, 0);
    end;
  end;
end;

end.

