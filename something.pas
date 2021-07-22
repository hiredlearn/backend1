SalesMovementExport.pas file
// This is the event that will be triggered when the conference button is clicked
procedure TFormMovementSalesExport.btnConferenceClick(Sender: TObject);
[...]
  // This is a class that facilitates coding against the database
  EntityConcurrence := TEntityConcurrence.Create(trCad);
  try
    // Here is the table of the operation being treated
    EntityConcurrence.GetModel.Table.Value := 'SALE';
    // This is the operation being treated - Order Checking
    EntityConcurrence.GetModel.Operation.Value := 'SALE CONFERENCE';
    // This is the key of the registry being treated at the table before mentioned
    EntityConcurrence.GetModel.Key.Value := Query.FieldByName('VIEW_CODE').AsInteger;
    // Here is being checked in the database if there is a 
    // treatment being made on this operation over this registry
    // This is the part responsible to check if there is someone
    // checking this order currently
    EntityConcurrence.ReadBySpecificFeilds(
      [
        EntityConcurrence.GetModel.Table,
        EntityConcurrence.GetModel.Operation,
        EntityConcurrence.GetModel.Key
      ]
    );
    // If there is (Leu is Read, that it was found in the database)
    if EntityConcurrence.Read then
    begin
      // If it was found the user will be warned and asked if he wants to cancel
      // the checking that other user initiated at other moment (there is a situation
      // covered here that is when a user begins the order checking and doesn't finish it
      // maybe turning off the computer, there will be a registry in the database but this
      // doesn't mean that the other user is currently checking the order)
      if not dlgQuestion(
        Self,
        Format(
          'There is a user-initiated unfinished conference. Do you want to invalidate it and continue?',
          [
            // From the USU_CODIGO is retrieved the user name (USU_NOME) from table USUARIO (USER)
            MDB.Search('USER', 'USER_CODE', 'USERNAME', EntityConcurrence.GetModel.User.Value, trCad),
            // Here is the moment that the order checking initiated from the other user
            EntityConcurrence.GetModel.Data.AsString
          ]
        )
      ) then
      begin
        // If the user doens't want to cancel the other checking the process won't continue
        Exit;
      end;
      // If the user answers that he wants to cancel the other checking, the other checking will be
      // erased from the database
      EntityConcurrence.Delete;
    end;
    // The fields are cleared and reevaluted
    EntityConcurrence.CleanModel;
    EntityConcurrence.GetModel.Table.Value := 'SALE';
    EntityConcurrence.GetModel.Operation.Value := 'SALE CONFERENCE';
    EntityConcurrence.GetModel.Key.Value := Query.FieldByName('SALE_CODE').AsInteger;
    EntityConcurrence.GetModel.User.Value := CurrentUser;
    // It is stored in the database
    EntityConcurrence.Record;
    EntityConcurrence.Transaction.CommitRetaining;
    CON_CODE := EntityConcurrence.Model.Code.Value;
  finally
    EntityConcurrence.Free;
  end;

  // Here the conference screen is instantiated
  FormConf:=TFormMovProductConference.Create(Nil);
  // A procedure that must be executed according to our framework
  FormConf.PerformBeforeShow;
  // Here is passed the concurrence code to a screen variable to be handled inside there
  FormConf.CON_CODE := CON_CODE;
[...]


untMovProductConference.pas file
TFormMovProductConference = class(TFormBasic)
[...]
public
  { Public declarations }
  CON_CODE: Integer;
[...]

// Here is the code that will be executed when the cancel button is clicked
// on in the conference screen
procedure TFormMovProductConference.acCancelExecute(Sender: TObject);
var
  EntityConcurrence: TEntityConcurrence;
begin
  ModalResult:=mrCancel;
  EntityConcurrence := TEntityConcurrence.Create(TRRegister);
  try
    // When canceled the concurrence in the database will be deleted
    EntityConcurrence.Delete(CON_CODE);
  finally
    EntityConcurrence.Free;
  end;
end;

// Here is the code executed when the save button is clicked
procedure TFormMovProductConference.btnRecordClick(Sender: TObject);
@@ -732,7 +742,44 @@ procedure TFormMovProductConference.btnRecordClick(Sender: TObject);
  end;
var
  TreatsPackage,MODULE_PACKAGE,AlteredBatch: Boolean;
  EntityConcurrence: TEntityConcurrence;
begin
  // As explained before
  EntityConcurrence := EntityConcurrence.Create(TRRegister);
  try
    EntityConcurrence.GetModel.Table.Value := 'SALE';
    EntityConcurrence.GetModel.Operation.Value := 'SALE CONFERENCE';
    EntityConcurrence.GetModel.Key.Value := SALE_CODE;
    EntityConcurrence.ReadBySpecificFeilds(
      [
        EntidadeConcorrencia.GetModel.Table,
        EntidadeConcorrencia.GetModel.Operation,
        EntityConcurrence.GetModel.Key
      ]
    );
    // If there ins't a registry in the database that represents the checking of this order
    // then it means someone canceled this checking and the checking that he/ she started
    if EntityConcurrence.Model.Code.IsEmpty then
    begin
      // In this case the user is warned and can't continue the process
      // Only cancel it and start again
      dlgNotice('Invalid conference. Please close the screen and start the sale conference again.');
      Exit;
    end;
    // If there is a registry it is verified if is the same as the current one the user is doing
    if EntityConcurrence.Model.Code.Value <> CON_CODE then
    begin
      // If it ins't the user is reported that the current checking was invalidated by the user X at moment Y
      // And that he/ she can only cancel the checking and start again
      dlgNotice(
        Self,
        Format(
          'Invalid User Conference. Please close the screen and start the sale conference again.',
          [
            MDB.Search('USER', 'USER_CODE', 'USERNAME', EntityConcurrence.GetModel.User.Value, TRRegistration),
            EntityConcurrence.GetModel.Data.AsString
          ]
        )
      );
      Exit;
    end;
    // If it ins't the case for any above situations the registry is deleted
    EntityConcurrence.Delete;
  finally
    EntityConcurrence.Free;
  end;
