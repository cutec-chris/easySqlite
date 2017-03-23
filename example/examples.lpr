program examples;
{$MODE ObjFpc}
{$H+}
{$M+}

uses
  Classes, SysUtils, Contnrs, Fgl,
  (* easySqlite units *)
  EasySqlite,EasySqliteOrm,
  (* project units *)
  Model;

var
  Connector: TSqliteConnector;
  Mapper: TSqliteAutoMapper;

procedure InsertCustomer;
var
  St: TSqliteStatement;
begin
  try
    St := Connector.Prepare('INSERT INTO Customers (Name, Address) VALUES (?, ?)')
                   .BindParam('Seven of Nine')
                   .BindParam('Astrometrics, Voyager');
    if St.Execute then
      WriteLn(Format('Affected Rows: %d', [St.AffectedRows]))
    else
      WriteLn(St.ErrorMessage)
  finally
    FreeAndNil(St);
  end;
end;

procedure InsertTwoCustomers;
var
  St: TSqliteStatement;
begin
  try
    St := Connector.Prepare('INSERT INTO Customers (Name, Address) VALUES (?, ?)');

    St.BindParam('Harry Kim')
      .BindParam('Navigation, Voyager')
      .Execute;
    WriteLn(Format('Affected Rows: %d', [St.AffectedRows]));

    St.ResetStatement
      .BindParam('Tuvok')
      .BindParam('Weapons Section, Voyager')
      .Execute;
    WriteLn(Format('Affected Rows: %d', [St.AffectedRows]));
  finally
    FreeAndNil(St);
  end;
end;

procedure SelectCustomers;
var
  St: TSqliteStatement;
begin
  try
    St := Connector.Prepare('SELECT * FROM customers WHERE Name <> ?')
                   .BindParam('Seven of Nine');
    if St.Execute then begin
      while St.Fetch do begin
        WriteLn(Format('%d | %s | %s', [St.Integers(0), St.Strings(1), St.Strings(2)]));
      end;
    end else
      WriteLn(St.ErrorMessage);
  finally
    FreeAndNil(St);
  end;
end;

procedure SelectCustomersNamed;
var
  St: TSqliteStatement;
begin
  try
    St := Connector.Prepare('SELECT * FROM customers WHERE Name <> @Name')
                   .BindParam('@Name', 'Seven of Nine');
    if St.Execute then begin
      while St.Fetch do begin
        WriteLn(Format('%d | %s | %s', [St.Integers('Id') , St.Strings('Name'), St.Strings('Address')]));
      end;
    end else
      WriteLn(St.ErrorMessage);
  finally
    FreeAndNil(St);
  end;
end;

procedure UpdateCustomers;
var
  St: TSqliteStatement;
begin
  try
    St := Connector.Prepare('UPDATE customers SET Address = @NewAddress WHERE Address LIKE @OldAddress')
                   .BindParam('@OldAddress', '%Voyager')
                   .BindParam('@NewAddress', 'Enterprise');
   if St.Execute then begin
      WriteLn(Format('Affected Rows: %d', [St.AffectedRows]));
    end else
      WriteLn(St.ErrorMessage);
  finally
    FreeAndNil(St);
  end;
end;

procedure DeleteCustomer;
var
  St: TSqliteStatement;
begin
  try
    St := Connector.Prepare('DELETE FROM customers WHERE Name LIKE @Name')
                   .BindParam('@Name', 'Harry Kim');
   if St.Execute then begin
      WriteLn(Format('Affected Rows: %d', [St.AffectedRows]));
    end else
      WriteLn(St.ErrorMessage);
  finally
    FreeAndNil(St);
  end;
end;

procedure InsertProductsAndStocks;
var
  St1, St2: TSqliteStatement;
  Product1, Product2: TProduct;
  Stock1, Stock2: TProductStock;
begin
  try
    St1 := Connector.Prepare('INSERT INTO products (Name, Description, Price) VALUES (@Name, @Description, @Price)');
    St2 := Connector.Prepare('INSERT INTO productstocks (ProductId, Count) VALUES (@ProductId, @Count)');

    Product1 := TProduct.Create;
    Product1.Name := 'Warp Core';
    Product1.Description := 'Standard Warp-6 Core';
    Product1.Price := 520.50;
    Mapper.BindObjectToParams(St1, Product1);
    if St1.Execute then begin
      Product1.Id := St1.LastInsertRow;
      WriteLn(Format('Affected Rows: %d', [St1.AffectedRows]));
    end;

    Stock1 := TProductStock.Create;
    Stock1.ProductId := Product1.Id;
    Stock1.Count := 42;
    Mapper.BindObjectToParams(St2, Stock1);
    if St2.Execute then begin
      Stock1.Id := St2.LastInsertRow;
      WriteLn(Format('Affected Rows: %d', [St2.AffectedRows]));
    end;

    St1.ResetStatement;
    St2.ResetStatement;

    Product2 := TProduct.Create;
    Product2.Name := 'Transporter Cabin';
    Product2.Description := 'Six Persons Standard Transporter';
    Product2.Price := 317.33;
    Mapper.BindObjectToParams(St1, Product2);
    if St1.Execute then begin
      Product2.Id := St1.LastInsertRow;
      WriteLn(Format('Affected Rows: %d', [St1.AffectedRows]));
    end;

    Stock2 := TProductStock.Create;
    Stock2.ProductId := Product2.Id;
    Stock2.Count := 23;
    Mapper.BindObjectToParams(St2, Stock2);
    if St2.Execute then begin
      Stock2.Id := St2.LastInsertRow;
      WriteLn(Format('Affected Rows: %d', [St2.AffectedRows]));
    end;
  finally
    FreeAndNil(Stock2);
    FreeAndNil(Product2);
    FreeAndNil(Stock1);
    FreeAndNil(Product1);
    FreeAndNil(St2);
    FreeAndNil(St1);
  end;
end;

procedure SelectProduct;
var
  St: TSqliteStatement;
  Product: TProduct;
begin
  try
    St := Connector.Prepare('SELECT * FROM products WHERE Name = @Name')
                   .BindParam('@Name', 'Warp Core');
    Product := Mapper.ExecuteStatementAsObject(St, TProduct) as TProduct;
    Product.PrintContent;
  finally
    FreeAndNil(Product);
    FreeAndNil(St);
  end;
end;

procedure SelectProductList;
var
  St: TSqliteStatement;
  List: TObjectList;
  p: Pointer;
begin
  try
    St := Connector.Prepare('SELECT * FROM products');
    List := TObjectList.Create(True);
    Mapper.ExecuteStatementAndAppendList(St, TProduct, List);
    for p in List do begin
      TProduct(p).PrintContent;
    end;
  finally
    FreeAndNil(List);
    FreeAndNil(St);
  end;
end;

procedure UpdateProductStock;
var
  St1, St2: TSqliteStatement;
  ProductStock: TProductStock;
begin
  try
    St1 := Connector.Prepare('SELECT productstocks.Id, productstocks.ProductId, productStocks.Count FROM productstocks ' +
                             'INNER JOIN products ON products.Id = productstocks.ProductId WHERE products.Name = @ProductName')
                    .BindParam('@ProductName', 'Warp Core');
    ProductStock := Mapper.ExecuteStatementAsObject(St1, TProductStock) as TProductStock;
    ProductStock.Count := ProductStock.Count - 5;
    St2 := Connector.Prepare('UPDATE productstocks SET Count=@Count WHERE Id=@Id');
    Mapper.BindObjectToParams(St2, ProductStock);
    if St2.Execute then
      WriteLn(Format('Affected Rows: %d', [St2.AffectedRows]));
  finally
    FreeAndNil(ProductStock);
    FreeAndNil(St2);
    FreeAndNil(St1);
  end;
end;

procedure SelectStockedProduct;
var
  St: TSqliteStatement;
  StockedProduct: TStockedProduct;
begin
  try
    St := Connector.Prepare('SELECT products.Id, products.Name, products.Description, products.Price, productStocks.Count FROM products ' +
                            'INNER JOIN productstocks ON products.Id = productstocks.ProductId WHERE products.Name = @productName')
                   .BindParam('@ProductName', 'Warp Core');
    StockedProduct := Mapper.ExecuteStatementAsObject(St, TStockedProduct) as TStockedProduct;
    StockedProduct.PrintContent;
  finally
    FreeAndNil(StockedProduct);
    FreeAndNil(St);
  end;
end;

procedure SelectProductGenericList;
var
  St: TSqliteStatement;
  List: TStockedProductList;
  o: TStockedProduct;
begin
  try
    St := Connector.Prepare('SELECT products.Id, products.Name, products.Description, products.Price, productStocks.Count FROM products ' +
                            'INNER JOIN productstocks ON products.Id = productstocks.ProductId');
    List := TStockedProductList.Create(True);
    Mapper.ExecuteStatementAndAppendList(St, TStockedProduct, List);
    for o in List do
      o.PrintContent;
  finally
    FreeAndNil(List);
    FreeAndNil(St);
  end;
end;

begin
  WriteLn(LineEnding + '== setup connector ==');
  Connector := TSqliteConnector.Create('example.sqlite');
  with Connector.Prepare('DELETE FROM customers; DELETE FROM products; DELETE FROM productstocks') do begin
    Execute;
    Free;
  end;

  WriteLn(LineEnding + '== insert a customer ==');
  InsertCustomer;

  WriteLn(LineEnding + '== insert two customers ==');
  InsertTwoCustomers;

  WriteLn(LineEnding + '== select customers ==');
  SelectCustomers;

  WriteLn(LineEnding + '== update customers ==');
  UpdateCustomers;

  WriteLn(LineEnding + '== delete customer ==');
  DeleteCustomer;

  WriteLn(LineEnding + '== select customers, using named params and values ==');
  SelectCustomersNamed;

  WriteLn(LineEnding + LineEnding + '== setup mapper ==');
  Mapper := TSqliteAutoMapper.Create;

  WriteLn(LineEnding + '== insert products and stocks ==');
  InsertProductsAndStocks;

  WriteLn(LineEnding + '== select product ==');
  SelectProduct;

  WriteLn(LineEnding + '== select product list ==');
  SelectProductList;

  WriteLn(LineEnding + '== update product stock ==');
  UpdateProductStock;

  WriteLn(LineEnding + '== select stocked product');
  SelectStockedProduct;

  WriteLn(LineEnding + '== select stocked product list');
  SelectProductGenericList;

  WriteLn(LineEnding + '== shutdown ==');
  FreeAndNil(Connector);
end.

