unit UnitTrain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ToolWin, Vcl.ComCtrls, System.Actions, Vcl.ActnList, Vcl.Menus,
  System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.StdCtrls,
  System.Math, System.Types, System.Generics.Collections,
  UnitNeurons, UnitTrainers;

type
  TTrainerEventHandlers = class
    procedure btnLoadTrainersFromFileClick(Sender: TObject);
    procedure btnCalculateClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
  end;

procedure trainSetUp();

var
  btnLoadTrainersFromFile, btnCalculate, btnExecute: TButton;
  label1, label2, label3, label4, labelOut1, labelOut2,
    labelText1, labelText2: TLabel;
  trainerHandlers: TtrainerEventHandlers;
  trainerIndex: Integer;
    // array size is not smaller than possible max number of neurons
  neuronsActivationOrder: array[0..200] of Integer;
  activOrderIndex: Integer;
  firstPass: Boolean;
  trainTabVisited: Integer;
  logFile: TextFile;
  inputNum, outputNum, dataSetSize: Integer;
  strList: TStringList;
  learningRate: Double;
  minError: Double;
  minErrorChange: Double;
  maxEpochs: Integer;
  currentError, lastError: Double;
  epochNumber: Integer;

const
  inputShift = 6;
  outputShift = 11;
  blockSize = 9;

implementation

uses
  MForm;

procedure TTrainerEventHandlers.btnLoadTrainersFromFileClick(Sender:
  TObject);
var
  i, j: Integer;
begin
  strList := TStringList.Create;
  strList.Sorted := false;
  if FileExists('or1.txt') then
    strList.LoadFromFile('or1.txt')
  else
    ShowMessage('Файл не существует');

  inputNum := StrToInt(strList[strList.indexOf('inputs:') + 1]);
  outputNum := StrToInt(strList[strList.indexOf('outputs:') + 1]);
  dataSetSize := StrToInt(strList[strList.indexOf('dataset size:')
    + 1]);

  for i := 0 to dataSetSize - 1 do
  begin
    trainers[i] := TTrainer.Create;
    for j := 1 to inputNum do
      trainers[i].inputs[j - 1] := strToInt(strList[i * blockSize
        + j + inputShift]);

    for j := 1 to outputNum do
      trainers[i].outputs[j - 1] := strToInt(strList[i *
        blockSize + j + outputShift]);
  end;
  ShowMessage('Загружено');
end;

// initialize outputValue
// for input neurons and bias neurons
procedure initializeOutputsOfNeurons();
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to neuronUnitsCount - 1 do
  begin
    if neurons[i] is TInpNeuron then
    begin
      neurons[i].outputValue := trainers[trainerIndex].inputs[j];
      Inc(j);
    end
    else if (neurons[i] is TBiasNeuron) and (firstPass = True) then
      neurons[i].outputValue := 1;
  end;
end;

procedure addToSequence(neuronIndex: Integer);
begin
  neuronsActivationOrder[activOrderIndex] := neuronIndex;
  Inc(activOrderIndex);
end;

function checkNotInTheSequenceYet(neuronIndex: Integer): Boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 0 to activOrderIndex - 1 do
  begin
    if neuronsActivationOrder[i] = neuronIndex then
      Result := false;
  end;
end;

  // Mark visited neurons as important (for network results)
  // If visited neuron is fully calculatable, then
  // then add its index to neuronsActivationOrder[]
  // bfs - breadth-first search algorithm
procedure bfs(firstNeuron: TOutNeuron);
var
  queue: TQueue<TNeuronUnit>;
  thisNeuron: TNeuronUnit;
  i: Integer;
begin
  queue := TQueue<TNeuronUnit>.Create;

  if checkNotInTheSequenceYet(firstNeuron.index) = true and
    firstNeuron.checkIfCalculatable(true) = true then
    addToSequence(firstNeuron.index);

  firstNeuron.isImportant := true;
  queue.Enqueue(firstNeuron);
  while queue.Count <> 0 do
  begin
    thisNeuron := queue.Extract;
    for i := 0 to conCount - 1 do
      if connections[i]._to = thisNeuron.index then
      begin

        if checkNotInTheSequenceYet(thisNeuron.index) = true and
          thisNeuron.checkIfCalculatable(true) = true then
          addToSequence(thisNeuron.index);

        thisNeuron.isImportant := true;
        queue.Enqueue(neurons[connections[i]._from]);
      end;
  end;
  queue.Free;
end;

function addDependentNeurons(): Boolean;
var
  i, j: Integer;
begin
  Result := false;
  for i := 0 to activOrderIndex - 1 do
    for j := 0 to conCount - 1 do
      // this neuron is beginning of a connection
      if (neuronsActivationOrder[i] = connections[j]._from) and
      // this neuron is already calculated
        (neurons[neuronsActivationOrder[i]].isCalculated = true) and
      // neuron on the end of this connection is not yet added to the list
        (checkNotInTheSequenceYet(connections[j]._to) = true) and
      // neuron on the end of this connection can be calculated
        (neurons[connections[j]._to].checkIfCalculatable(false) =
        true) then
      begin
        addToSequence(connections[j]._to);
        Result := true;
      end;
end;

// Besides activation of neurons, this procedure
// also defines order of activation of neurons on training,
// saving indices of activated neurons in array.
// It allows for activation of neurons on training
// in forwardPass() without calculating order of their activation.
procedure firstForwardPass();
var
  weightedSum: Double;
  haveNeuronsToActivate: Boolean;
  i: Integer;
  thisNeuron: TNeuronUnit;
begin
  for i := 0 to neuronUnitsCount - 1 do
    if neurons[i] is TOutNeuron then
      bfs(neurons[i] as TOutNeuron);
  repeat
  // calculate all not yet calculated neurons
    for i := 0 to activOrderIndex - 1 do
      if neurons[neuronsActivationOrder[i]].isCalculated = false then
      begin
        thisNeuron := neurons[neuronsActivationOrder[i]];
        weightedSum := thisNeuron.sumWeightedInputs();
        thisNeuron.outputValue := thisNeuron.Activate(weightedSum);
        thisNeuron.isCalculated := true;
      end;
  until addDependentNeurons() = false;
end;

// For second and further training passes
procedure forwardPass();
var
  i: Integer;
  weightedSum: Double;
  thisNeuron: TNeuronUnit;
begin
  for i := 0 to activOrderIndex - 1 do
  begin
    thisNeuron := neurons[neuronsActivationOrder[i]];
    weightedSum := thisNeuron.sumWeightedInputs();
    thisNeuron.outputValue := thisNeuron.Activate(weightedSum);
  end;
end;


// Partial derivative of loss function.
// Used only on the last layer of the network
// (that is, for output neurons).
function partialDerivativeError(i, outNeuronNum: Integer): Double;
begin
  result := 0.0;
  if neurons[neuronsActivationOrder[i]] is TOutNeuron then

    result := neurons[neuronsActivationOrder[i]].outputValue -
      trainers[trainerIndex].outputs[outNeuronNum];
end;

procedure backpropLastLayer();
var
  i, outNeuronNum: Integer;
  thisNeuron: TNeuronUnit;
begin
  outNeuronNum := 0;
  for i := 0 to activOrderIndex - 1 do
  begin
    thisNeuron := neurons[neuronsActivationOrder[i]];
    if thisNeuron is TOutNeuron then
    begin
      thisNeuron.dEdz := partialDerivativeError(i, outNeuronNum)
        * thisNeuron.derivativeOfSigmoid(thisNeuron.outputValue);
      Inc(outNeuronNum);
    end;
  end;

  for i := 0 to conCount - 1 do
    if (neurons[connections[i]._to] is TOutNeuron) then
    begin
      connections[i].nabla := learningRate * neurons[connections[i]._to].dEdz
        * neurons[connections[i]._from].outputValue;
    end;
end;

procedure backpropHiddenLayers();
var
  i, j, k: Integer;
  thisNeuron: TNeuronUnit;
begin

  for i := activOrderIndex - 1 downto 0 do
  begin
    thisNeuron := neurons[neuronsActivationOrder[i]];
    if thisNeuron is TNeuron then
    begin

      for j := 0 to conCount - 1 do
        if thisNeuron.index = connections[j]._from then
          thisNeuron.dEdz := thisNeuron.dEdz + neurons[connections
            [j]._to].dEdz * connections[j].weight;

            // Multiply on derivative of activation function
      thisNeuron.dEdz := thisNeuron.dEdz * thisNeuron.derivativeOfSigmoid
        (thisNeuron.outputValue);



        // update weights
      for k := 0 to conCount - 1 do
        if thisNeuron.index = connections[k]._to then
        begin
          connections[k].nabla := connections[k].nabla +
            thisNeuron.dEdz * learningRate * neurons[connections[k]._from].outputValue;
        end;
    end;
  end;
end;

procedure updateWeights();
var
  i: Integer;
begin
  for i := 0 to conCount - 1 do
  begin
    connections[i].weight := connections[i].weight - connections[i].nabla;
    connections[i].nabla := 0;
  end;
end;

procedure resetdEdz();
var
  i: Integer;
begin
  for i := 0 to activOrderIndex - 1 do
    neurons[neuronsActivationOrder[i]].dEdz := 0.0;
end;

procedure logWeights();
var
  i: Integer;
begin
  Writeln(logFile, 'outer neurons:');
  for i := 0 to conCount - 1 do
    if (neurons[connections[i]._to] is TOutNeuron) then
      Write(logFile, FloatToStrf(connections[i].weight -
        connections[i].nabla, ffFixed, 5, 5) + ' ');
  Writeln(logFile, slinebreak);
  Writeln(logFile, 'neurons of hidden layers:');
  for i := 0 to conCount - 1 do
    if (neurons[connections[i]._to] is TNeuron) then
      Write(logFile, FloatToStrf(connections[i].weight -
        connections[i].nabla, ffFixed, 5, 5) + ' ');
  Writeln(logFile, slinebreak);
end;

procedure TTrainerEventHandlers.btnCalculateClick(Sender: TObject);
var
  i: Integer;
begin

  if conCount = 0 then
  begin
    ShowMessage('Нейросеть отсутствует');
    exit;
  end;

  AssignFile(logFile, 'logFile.log');
  Rewrite(logFile);

    // initialize weights
  for i := 0 to conCount - 1 do
    connections[i].weight := RandomRange(-100, 101) * 0.01;

  for i := 0 to neuronUnitsCount - 1 do
    neurons[i].outputValue := 999;

  for i := 0 to Length(neuronsActivationOrder) - 1 do
    neuronsActivationOrder[i] := -1;
  while (epochNumber < MaxEpochs) do
  begin
    Writeln(logFile, 'epoch#' + IntToStr(epochNumber + 1));
    repeat

      initializeOutputsOfNeurons();

      if firstPass = True then
        firstForwardPass()
      else
        forwardPass();

      resetdEdz();

      backpropLastLayer();

      backpropHiddenLayers();

      updateWeights();

      firstPass := false;
      Inc(trainerIndex);

    until trainerIndex > Length(trainers) - 1;
    logWeights();
    trainerIndex := 0;
    Inc(epochNumber);
  end;
  ShowMessage('Сеть обучена');
  CloseFile(logFile);
end;

procedure initializeRandomOutputsOfNeurons();
var
  i: Integer;
begin
  for i := 0 to neuronUnitsCount - 1 do
  begin
    if neurons[i] is TInpNeuron then
      neurons[i].outputValue := Random(2);
  end;
end;

procedure executeForwardPass();
var
  i, count: Integer;
begin
  initializeRandomOutputsOfNeurons();
  forwardPass();
  labelText1.caption := 'Значения на входах сети';
  count := 1;
  for i := 0 to neuronUnitsCount - 1 do
    if neurons[i] is TInpNeuron then
    begin
      case count of
        1:
          label1.caption := FloatToStrF(neurons[i].outputValue,
            ffFixed, 4, 4);
        2:
          label2.caption := FloatToStrF(neurons[i].outputValue,
            ffFixed, 4, 4);
        3:
          label3.caption := FloatToStrF(neurons[i].outputValue,
            ffFixed, 4, 4);
        4:
          label4.caption := FloatToStrF(neurons[i].outputValue,
            ffFixed, 4, 4);
      end;
      Inc(count);
    end;

  labelText2.caption := 'Значения на выходах сети';
  count := 1;
  for i := 0 to neuronUnitsCount - 1 do
    if neurons[i] is TOutNeuron then
    begin
      case count of
        1:
          labelOut1.caption := FloatToStrF(neurons[i].outputValue,
            ffFixed, 4, 4);
        2:
          labelOut2.caption := FloatToStrF(neurons[i].outputValue,
            ffFixed, 4, 4);
      end;
      Inc(count);
    end;
end;

procedure TTrainerEventHandlers.btnExecuteClick(Sender: TObject);
begin
  if conCount = 0 then
  begin
    ShowMessage('Нейросеть отсутствует');
    exit;
  end;

  executeForwardPass();
end;

procedure trainSetUp();
begin
  trainerHandlers := TTrainerEventHandlers.Create;
  btnLoadTrainersFromFile := TButton.Create(FormMain);
  with btnLoadTrainersFromFile do
  begin
    Parent := FormMain.MainPanel;
    Left := 120;
    Top := 70;
    Height := 55;
    Width := 90;
    Caption := 'Выбрать файл с обучающей выборкой';
    WordWrap := true;
    OnClick := trainerHandlers.btnLoadTrainersFromFileClick;
  end;
  btnCalculate := TButton.Create(FormMain);
  with btnCalculate do
  begin
    Parent := FormMain.MainPanel;
    Left := 120;
    Top := 160;
    Height := 25;
    Width := 90;
    Caption := 'Обучить';
    OnClick := trainerHandlers.btnCalculateClick;
  end;
  btnExecute := TButton.Create(FormMain);
  with btnExecute do
  begin
    Parent := FormMain.MainPanel;
    Left := 120;
    Top := 220;
    Height := 25;
    Width := 90;
    Caption := 'Тест';
    OnClick := trainerHandlers.btnExecuteClick;
  end;

  label1 := TLabel.Create(FormMain);
  label2 := TLabel.Create(FormMain);
  label3 := TLabel.Create(FormMain);
  label4 := TLabel.Create(FormMain);
  labelOut1 := TLabel.Create(FormMain);
  labelOut2 := TLabel.Create(FormMain);
  labelText1 := TLabel.Create(FormMain);
  labelText2 := TLabel.Create(FormMain);
  with label1 do
  begin
    Parent := FormMain.MainPanel;
    Left := 350;
    Top := 150;
    Caption := '';
    Font.Size := 12;
  end;
  with label2 do
  begin
    Parent := FormMain.MainPanel;
    Left := 420;
    Top := 150;
    Caption := '';
    Font.Size := 12;
  end;
  with label3 do
  begin
    Parent := FormMain.MainPanel;
    Left := 490;
    Top := 150;
    Caption := '';
    Font.Size := 12;
  end;
  with label4 do
  begin
    Parent := FormMain.MainPanel;
    Left := 560;
    Top := 150;
    Caption := '';
    Font.Size := 12;
  end;
  with labelOut1 do
  begin
    Parent := FormMain.MainPanel;
    Left := 380;
    Top := 250;
    Caption := '';
    Font.Size := 12;
  end;
  with labelOut2 do
  begin
    Parent := FormMain.MainPanel;
    Left := 490;
    Top := 250;
    Caption := '';
    Font.Size := 12;
  end;
  with labelText1 do
  begin
    Parent := FormMain.MainPanel;
    Left := 350;
    Top := 120;
    Caption := '';
    Font.Size := 12;
  end;
  with labelText2 do
  begin
    Parent := FormMain.MainPanel;
    Left := 350;
    Top := 220;
    Caption := '';
    Font.Size := 12;
  end;
  if trainTabVisited <> 100 then
  begin
    activOrderIndex := 0;
    trainerIndex := 0;
    firstPass := True;
    learningRate := 10.0;
  end;
  trainTabVisited := 100;
  inputNum := 0;
  outputNum := 0;
  currentError := Double.MaxValue;
  lastError := 0;
  epochNumber := 0;
  maxEpochs := 100;
end;

end.

