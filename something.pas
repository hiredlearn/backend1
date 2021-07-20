UntMovimentoVendasExportacao.pas file
// This is the event that will be triggered when the conference button is clicked
procedure TFormMovimentoVendasExportacao.btnConferenciaClick(Sender: TObject);
[...]
  // This is a class that facilitates coding against the database
  EntidadeConcorrencia := TEntidadeConcorrencia.Create(trCad);
  try
    // Here is the table of the operation being treated
    EntidadeConcorrencia.GetModel.Tabela.Value := 'VENDA';
    // This is the operation being treated - Order Checking
    EntidadeConcorrencia.GetModel.Operacao.Value := 'CONFERENCIA VENDA';
    // This is the key of the registry being treated at the table before mentioned
    EntidadeConcorrencia.GetModel.Chave.Value := qryConsulta.FieldByName('VD_CODIGO').AsInteger;
    // Here is being checked in the database if there is a 
    // treatment being made on this operation over this registry
    // This is the part responsible to check if there is someone
    // checking this order currently
    EntidadeConcorrencia.LerPorCamposEspecificos(
      [
        EntidadeConcorrencia.GetModel.Tabela,
        EntidadeConcorrencia.GetModel.Operacao,
        EntidadeConcorrencia.GetModel.Chave
      ]
    );
    // If there is (Leu is Read, that it was found in the database)
    if EntidadeConcorrencia.Leu then
    begin
      // If it was found the user will be warned and asked if he wants to cancel
      // the checking that other user initiated at other moment (there is a situation
      // covered here that is when a user begins the order checking and doesn't finish it
      // maybe turning off the computer, there will be a registry in the database but this
      // doesn't mean that the other user is currently checking the order)
      if not dlgPergunta(
        Self,
        Format(
          'Há uma conferência não finalizada iniciada pelo usuário %s às %s. Deseja invalida-la e continuar?',
          [
            // From the USU_CODIGO is retrieved the user name (USU_NOME) from table USUARIO (USER)
            MDB.Busca('USUARIO', 'USU_CODIGO', 'USU_NOME', EntidadeConcorrencia.GetModel.Usuario.Value, trCad),
            // Here is the moment that the order checking initiated from the other user
            EntidadeConcorrencia.GetModel.Data.AsString
          ]
        )
      ) then
      begin
        // If the user doens't want to cancel the other checking the process won't continue
        Exit;
      end;
      // If the user answers that he wants to cancel the other checking, the other checking will be
      // erased from the database
      EntidadeConcorrencia.Excluir;
    end;
    // The fields are cleared and reevaluted
    EntidadeConcorrencia.LimparModel;
    EntidadeConcorrencia.GetModel.Tabela.Value := 'VENDA';
    EntidadeConcorrencia.GetModel.Operacao.Value := 'CONFERENCIA VENDA';
    EntidadeConcorrencia.GetModel.Chave.Value := qryConsulta.FieldByName('VD_CODIGO').AsInteger;
    EntidadeConcorrencia.GetModel.Usuario.Value := UsuarioAtual;
    // It is stored in the database
    EntidadeConcorrencia.Gravar;
    EntidadeConcorrencia.Transaction.CommitRetaining;
    CON_CODIGO := EntidadeConcorrencia.Model.Codigo.Value;
  finally
    EntidadeConcorrencia.Free;
  end;

  // Here the conference screen is instantiated
  FormConf:=TFormMovConferenciaProduto.Create(Nil);
  // A procedure that must be executed according to our framework
  FormConf.ExecutarAntesShow;
  // Here is passed the concurrence code to a screen variable to be handled inside there
  FormConf.CON_CODIGO := CON_CODIGO;
[...]


untMovConferenciaProduto.pas file
TFormMovConferenciaProduto = class(TFormBasico)
[...]
public
  { Public declarations }
  CON_CODIGO: Integer;
[...]

// Here is the code that will be executed when the cancel button is clicked
// on in the conference screen
procedure TFormMovConferenciaProduto.acCancelarExecute(Sender: TObject);
var
  EntidadeConcorrencia: TEntidadeConcorrencia;
begin
  ModalResult:=mrCancel;
  EntidadeConcorrencia := TEntidadeConcorrencia.Create(TRCadastro);
  try
    // When canceled the concurrence in the database will be deleted
    EntidadeConcorrencia.Excluir(CON_CODIGO);
  finally
    EntidadeConcorrencia.Free;
  end;
end;

// Here is the code executed when the save button is clicked
procedure TFormMovConferenciaProduto.btnGravarClick(Sender: TObject);
@@ -732,7 +742,44 @@ procedure TFormMovConferenciaProduto.btnGravarClick(Sender: TObject);
  end;
var
  TrataPacote,MODULO_PACOTE,AlterouLote: Boolean;
  EntidadeConcorrencia: TEntidadeConcorrencia;
begin
  // As explained before
  EntidadeConcorrencia := TEntidadeConcorrencia.Create(TRCadastro);
  try
    EntidadeConcorrencia.GetModel.Tabela.Value := 'VENDA';
    EntidadeConcorrencia.GetModel.Operacao.Value := 'CONFERENCIA VENDA';
    EntidadeConcorrencia.GetModel.Chave.Value := VD_CODIGO;
    EntidadeConcorrencia.LerPorCamposEspecificos(
      [
        EntidadeConcorrencia.GetModel.Tabela,
        EntidadeConcorrencia.GetModel.Operacao,
        EntidadeConcorrencia.GetModel.Chave
      ]
    );
    // If there ins't a registry in the database that represents the checking of this order
    // then it means someone canceled this checking and the checking that he/ she started
    if EntidadeConcorrencia.Model.Codigo.IsEmpty then
    begin
      // In this case the user is warned and can't continue the process
      // Only cancel it and start again
      dlgAviso('Conferência inválidada. Por favor feche a tela e comece a conferência da venda novamente');
      Exit;
    end;
    // If there is a registry it is verified if is the same as the current one the user is doing
    if EntidadeConcorrencia.Model.Codigo.Value <> CON_CODIGO then
    begin
      // If it ins't the user is reported that the current checking was invalidated by the user X at moment Y
      // And that he/ she can only cancel the checking and start again
      dlgAviso(
        Self,
        Format(
          'Conferência inválidada pelo usuário %s às %s. Por favor feche a tela e comece a conferência da venda novamente',
          [
            MDB.Busca('USUARIO', 'USU_CODIGO', 'USU_NOME', EntidadeConcorrencia.GetModel.Usuario.Value, TRCadastro),
            EntidadeConcorrencia.GetModel.Data.AsString
          ]
        )
      );
      Exit;
    end;
    // If it ins't the case for any above situations the registry is deleted
    EntidadeConcorrencia.Excluir;
  finally
    EntidadeConcorrencia.Free;
  end;
