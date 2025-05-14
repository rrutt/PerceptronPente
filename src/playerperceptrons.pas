unit playerperceptrons;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  perceptron;

type
  TPlayerPerceptrons = class
  private
  public
    Wins: integer;
    Losses: integer;
    Perceptrons: TPerceptronArray;
  end;

implementation

end.

