unit Model;
{$MODE ObjFpc}
{$H+}
{$M+}

interface

uses
  Classes, SysUtils, Fgl,
  (* easySqlite units *)
  EasySqliteOrm;

type
  TProduct = class(TObject)
    private
      FDescription: String;
      FId: Int64;
      FName: String;
      FPrice: Double;
    public
      procedure PrintContent;
    published
      property Id: Int64 read FId write FId;
      property Name: String read FName write FName;
      property Description: String read FDescription write FDescription;
      property Price: Double read FPrice write FPrice;
  end;

  TProductStock = class(TObject)
    private
      FCount: Int64;
      FId: Int64;
      FProductId: Int64;
    published
      property Id: Int64 read FId write FId;
      property ProductId: Int64 read FProductId write FProductId;
      property Count: Int64 read FCount write FCount;
  end;

  TStockedProduct = class(TProduct)
    private
      FCount: Int64;
    public
      procedure PrintContent;
    published
      property Count: Int64 read FCount write FCount;
  end;

  TStockedProductList = class(specialize TFPGObjectList<TStockedProduct>, IObjectContainer)
    private
      procedure AddObject(AnObject: TObject);
  end;

implementation

procedure TStockedProductList.AddObject(AnObject: TObject);
begin
  Self.Add(TStockedProduct(AnObject));
end;

procedure TStockedProduct.PrintContent;
begin
  WriteLn(Format('{"Id": %d, "Name": "%s", "Description": "%s", "Price": %f, "Count": %d}', [Id, Name, Description, Price, Count]));
end;

procedure TProduct.PrintContent;
begin
  WriteLn(Format('{"Id": %d, "Name": "%s", "Description": "%s", "Price": %f}', [Id, Name, Description, Price]));
end;

end.
