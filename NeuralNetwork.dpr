program NeuralNetwork;

uses
  Vcl.Forms,
  MForm in 'MForm.pas' {FormMain},
  Vcl.Themes,
  Vcl.Styles,
  UnitNeurons in 'UnitNeurons.pas',
  UnitTrain in 'UnitTrain.pas',
  UnitUse in 'UnitUse.pas',
  UnitTrainers in 'UnitTrainers.pas',
  UnitModel in 'UnitModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Sky');
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
