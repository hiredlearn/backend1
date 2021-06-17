// SaleItem is holds price values
// ASale holds item name and attributes
// SaleItem.sku is an indicator that the configurable comes AFTER (not before)

if SaleItem.product_type='simple' then
  begin
      if (I>0) and (ASale.items[I-1].product_type='configurable') then
      begin
        SaleItem:=ASale.items[I-1]; 
        //it changes the SaleItem but downwards it consider only the values of it, the name was handled upwards, before the if statement
      end else
      if (Length(ASale.items) > I + 1) and (ASale.items[I + 1].product_type = 'configurable') and (ASale.items[I + 1].sku = SaleItem.sku) then
      begin
        SaleItem:=ASale.items[I+1]; 
      end;
    End;

