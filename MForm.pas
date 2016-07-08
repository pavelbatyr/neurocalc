unit MForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ToolWin, Vcl.ComCtrls, System.Actions, Vcl.ActnList, Vcl.Menus,
  System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.StdCtrls,
  System.Math, System.Types, UnitModel, UnitTrain,
  UnitNeurons, UnitTrainers;

type
  TFormMain = class(TForm)
    ToolBar: TToolBar;
    Status: TStatusBar;
    Images: TImageList;
    MainMenu: TMainMenu;
    ActionList: TActionList;
    Open: TOpenDialog;
    Save: TSaveDialog;
    acNew: TAction;
    acOpen: TAction;
    acSave: TAction;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    nFile: TMenuItem;
    nNew: TMenuItem;
    nOpen: TMenuItem;
    nSave: TMenuItem;
    btnToModel: TButton;
    btnToTrain: TButton;
    ToolButton1: TToolButton;
    MainPanel: TPanel;
    procedure acNewExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure clearMainPanel();
    procedure btnToModelClick(Sender: TObject);
    procedure btnToTrainClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.acNewExecute(Sender: TObject); // Новый
var
  i: Integer;
begin
  Status.Panels.Items[0].Text := 'Новая модель';
  conCount := 0;
  for i := 0 to neuronUnitsCount - 1 do
    neurons[i].Free;
  neuronsCount := 0;
  inpNeuronsCount := 0;
  neuronUnitsCount := 0;
  if PBox <> nil then
    PBox.Refresh;
end;

procedure TFormMain.acOpenExecute(Sender: TObject); // Open file of a network
var
  f: TextFile;
  i, boolFlag, neuronType: integer;
begin
  if not Open.Execute then
    exit;
  clearMainPanel();
  AssignFile(f, open.fileName);
  try
    Reset(f);
    readln(f, neuronsCount);
    readln(f, inpNeuronsCount);
    readln(f, outNeuronsCount);
    readln(f, biasNeuronsCount);
    readln(f, neuronUnitsCount);
    readln(f, conCount);
    readln(f);
    for i := 0 to neuronUnitsCount - 1 do
    begin
      Readln(f, neuronType);
      case neuronType of
        0:
          neurons[i] := TInpNeuron.Create();
        1:
          neurons[i] := TNeuron.Create();
        2:
          neurons[i] := TBiasNeuron.Create();
        3:
          neurons[i] := TOutNeuron.Create();
      end;
      neurons[i].neuronType := neuronType;
      readln(f, neurons[i].x);
      readln(f, neurons[i].y);
      neurons[i].visX := neurons[i].x;
      neurons[i].visY := neurons[i].y;
      readln(f, neurons[i].index);
      Readln(f, boolFlag);
      if boolFlag = -1 then
        neurons[i].hasConnections := true
      else
        neurons[i].hasConnections := false;
    end;
    readln(f);
    for i := 0 to conCount - 1 do
    begin
      readln(f, connections[i].n1);
      readln(f, connections[i].n2);
    end;
    modelSetUp();
    btnToModel.Font.Style := [fsBold];
    btnToTrain.Font.Style := [];
  finally
    CloseFile(f);
  end;
  Status.Panels.Items[0].Text := Open.FileName;
end;

procedure TFormMain.acSaveExecute(Sender: TObject); // Save network to a file
var
  f: TextFile;
  i: Integer;
begin
  if not Save.Execute then
    exit
  else
  begin
    AssignFile(f, Save.fileName);
    try
      Rewrite(f);
      writeln(f, neuronsCount);
      Writeln(f, inpNeuronsCount);
      Writeln(f, outNeuronsCount);
      Writeln(f, biasNeuronsCount);
      Writeln(f, neuronUnitsCount);
      Writeln(f, conCount);
      Writeln(f);
      for i := 0 to neuronUnitsCount - 1 do
      begin
        Writeln(f, neurons[i].neuronType);
        Writeln(f, neurons[i].x);
        Writeln(f, neurons[i].y);
        Writeln(f, neurons[i].index);
        Writeln(f, boolToStr(neurons[i].hasConnections));
      end;
      Writeln(f);
      for i := 0 to conCount - 1 do
      begin
        Writeln(f, connections[i].n1);
        Writeln(f, connections[i].n2);
      end;
    finally
      CloseFile(f);
    end;
  end;

  Status.Panels.Items[0].Text := Save.FileName;
end;

// Erase all buttons when switching between Model and Train tabs
procedure TFormMain.clearMainPanel();
begin
  FreeAndNil(PBox);
  FreeAndNil(btnCreateInpNeuron);
  FreeAndNil(btnCreateNeuron);
  FreeAndNil(btnCreateOutNeuron);
  FreeAndNil(btnCreateBiasNeuron);
  FreeAndNil(btnCreateLink);
  FreeAndNil(btnRemoveNeuron);
  FreeAndNil(btnLoadTrainersFromFile);
  FreeAndNil(btnCalculate);
  FreeAndNil(btnExecute);
  FreeAndNil(label1);
  FreeAndNil(label2);
  FreeAndNil(label3);
  FreeAndNil(label4);
  FreeAndNil(labelOut1);
  FreeAndNil(labelOut2);
end;

procedure TFormMain.btnToModelClick(Sender: TObject);
begin
  clearMainPanel();
  modelSetUp();
  btnToModel.Font.Style := [fsBold];
  btnToTrain.Font.Style := [];
end;

procedure TFormMain.btnToTrainClick(Sender: TObject);
begin
  clearMainPanel();
  trainSetUp();
  btnToModel.Font.Style := [];
  btnToTrain.Font.Style := [fsBold];
end;


procedure TFormMain.FormCreate(Sender: TObject);
begin
  randomize;
  neuronsCount := 0;
  inpNeuronsCount := 0;
  outNeuronsCount := 0;
  biasNeuronsCount := 0;
  neuronUnitsCount := 0;
  conCount := 0;
  currentIndex := -1;

  with Open do
  begin
    Options := Options + [ofPathMustExist, ofFileMustExist];
    InitialDir := ExtractFilePath(Application.ExeName);
  end;
  with Save do
  begin
    InitialDir := ExtractFilePath(Application.ExeName);
  end;
  btnToModel.Font.Style := [fsBold];
  btnToTrain.Font.Style := [];
  modelSetUp();
end;

// Scaling PaintBox visual area
procedure TFormMain.FormMouseWheel(Sender: TObject; Shift:
  TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled:
  Boolean);
var
  myPoint: TPoint;
  i: Integer;
begin
  if PBox = nil then
    exit;
  myPoint := PBox.ScreenToClient(mousePos);
  // if the cursor is inside of PaintBox area
  if PtInRect(PBox.ClientRect, myPoint) then
  begin
    oldZoom := zoom;
    if WheelDelta > 0 then
      zoom := zoom + 0.04
    else if zoom > 0 then
      zoom := zoom - 0.04;
      // centering on axes
    offSetX := (zoom * offSetX - (zoom - oldZoom) * (PBox.Width
      div 2)) / oldZoom;
    offSetY := (zoom * offSetY - (zoom - oldZoom) * (PBox.Height
      div 2)) / oldZoom;
    for i := 0 to neuronUnitsCount - 1 do
    begin
      neurons[i].visX := Round((neurons[i].x + shiftX) * zoom + offsetX);
      neurons[i].visY := Round((neurons[i].y + shiftY) * zoom + offSetY);
    end;
    // Scaling neurons
    zoomedNeuronSize := Round(neuronSize * zoom);
    zoomedInpNeuronSize := Round(inpNeuronSize * zoom);
    PBox.Refresh;
  end;
end;

// Changing PaintBox size when window size changes
procedure TFormMain.FormResize(Sender: TObject);
begin
  if PBox = nil then
    Exit;
  PBWidth := FormMain.Width - 200;
  PBHeight := FormMain.Height - 200;
  PBox.Width := PBwidth;
  PBox.Height := PBheight;
end;

end.

