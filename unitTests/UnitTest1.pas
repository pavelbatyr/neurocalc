unit UnitTest1;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TNeuroCalcTest = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    // Test with TestCase Atribute to supply parameters.
    [Test]
    [TestCase('TestA', '1,2')]
    [TestCase('TestB', '3,4')]
    procedure Test2(const AValue1: Integer; const AValue2: Integer);
  end;

implementation

procedure TNeuroCalcTest.Setup;
begin
end;

procedure TNeuroCalcTest.TearDown;
begin
end;

procedure TNeuroCalcTest.Test1;
begin
Assert.Pass('This passes');
end;

procedure TNeuroCalcTest.Test2(const AValue1: Integer; const AValue2: Integer);
begin
Assert.Fail('This fails');
end;

initialization

TDUnitX.RegisterTestFixture(TNeuroCalcTest);

end.
