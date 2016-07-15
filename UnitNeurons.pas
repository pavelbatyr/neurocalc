unit UnitNeurons;

interface

type
  TNeuronUnit = class
    x, y, index: Integer;
    visX, visY: Integer;
    hasConnections: Boolean;
    startX, startY: Integer;

    //0 - input neuron
    //1 - regular neuron
    //2 - bias neuron
    //3 - output neuron
    neuronType: Integer;
    isImportant: Boolean;
    outputValue: Double;
    isCalculated: Boolean;

    //partial derivative of loss function
    dEdz: Double;
    function checkIfCalculatable(bfs: Boolean): Boolean;
    function sumWeightedInputs(): Double;
    function activate(weightedSum: Double): Double;
    function derivativeOfSigmoid(value: Double): Double;
  end;

  TNeuron = class(TNeuronUnit)
  end;

  TInpNeuron = class(TNeuronUnit)
  end;

  TOutNeuron = class(TNeuronUnit)
  end;

  TBiasNeuron = class(TNeuronUnit)
  end;

  TConnection = record
    _from, _to: Integer;
    weight: Double;
    nabla: Double;
  end;

var
  neurons: array[0..200] of TNeuronUnit;
  // storing indices of two neurons from the neurons array
  connections: array[0..500] of TConnection;
  neuronsCount, conCount, inpNeuronsCount, neuronUnitsCount,
    outNeuronsCount, biasNeuronsCount: Integer;

implementation

  // Check all connections of this neuron and ensure
  // that every important one of them has some signal value associated
  // (except for input- and bias-neurons)

  // This function is only used to check if
  // it's possible to add the neuron to the sequence
  // of neurons for calculation

function TNeuronUnit.checkIfCalculatable(bfs: Boolean): Boolean;
var
  i: Integer;
begin
  Result := false;
  if ((self is TOutNeuron) or (self is TNeuron)) and (self.isCalculated
    = False) then
  begin
    Result := true;
  // checking inputs from regular, important neurons and making sure
  // that they have already been calculated
    for i := 0 to conCount - 1 do
    begin
      if self.index = connections[i]._to then
        if bfs = false then
        begin
          // important neurons of previous layer are of TNeuron type
          if (neurons[connections[i]._from].isImportant = true) and
            (neurons[connections[i]._from].isCalculated = false) then
            Result := false;
        end
        else // for bfs()
 if (neurons[connections[i]._from] is TNeuron) then
          if (neurons[connections[i]._from].isCalculated = false) then
            Result := false;
    end;
  end;
end;

// Calculate weighted sum of inputs of a calculatable neuron
function TNeuronUnit.sumWeightedInputs(): Double;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to conCount - 1 do
  begin
    if self.index = connections[i]._to then
      if neurons[connections[i]._from] is TNeuron then
      begin
        if neurons[connections[i]._from].isImportant = true and
          neurons[connections[i]._from].isCalculated = true then
          result := result + neurons[connections[i]._from].outputValue
            * connections[i].weight;
      end
      else  // if <> TNeuron
        result := result + neurons[connections[i]._from].outputValue
          * connections[i].weight;
  end;

end;

// Sigmoid activation function
function TNeuronUnit.activate(weightedSum: Double): Double;
begin
  result := 1.0 / (1.0 + Exp(-weightedSum));
end;

// Derivative of sigmoid activation function
// Function parameter is already a result of activation
function TNeuronUnit.derivativeOfSigmoid(value: Double): Double;
begin
  result := value * (1 - value);
end;

end.

