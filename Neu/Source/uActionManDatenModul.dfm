object ActionManDataModule: TActionManDataModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 312
  Width = 577
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=D:\DXWork\Projekte\DelphiStammtisch\REST\Win32\Debug\Ac' +
        'tionManager.db'
      'LockingMode=Normal'
      'DriverID=SQLite')
    AfterConnect = FDConnection1AfterConnect
    Left = 80
    Top = 48
  end
  object FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink
    Left = 208
    Top = 56
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 336
    Top = 48
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT'
      '  C.*'
      'from'
      '  COMPANY C')
    Left = 344
    Top = 128
  end
  object qryAction: TFDQuery
    Connection = FDConnection1
    UpdateObject = updAction
    SQL.Strings = (
      'SELECT'
      '  A.*'
      'from'
      '  actionmanager_Action A')
    Left = 208
    Top = 128
  end
  object updAction: TFDUpdateSQL
    Connection = FDConnection1
    InsertSQL.Strings = (
      'INSERT INTO ACTIONMANAGER_ACTION'
      '(ACTION_NAME, ACTION_CALL, ACTION_TYPE, ACTION_CATEGORY, '
      '  ACTION_CREATED, ACTION_LASTCALL, ACTION_CALLCNT)'
      
        'VALUES (:NEW_ACTION_NAME, :NEW_ACTION_CALL, :NEW_ACTION_TYPE, :N' +
        'EW_ACTION_CATEGORY, '
      
        '  :NEW_ACTION_CREATED, :NEW_ACTION_LASTCALL, :NEW_ACTION_CALLCNT' +
        ');'
      
        'SELECT LAST_INSERT_AUTOGEN() AS IDENT, ACTION_NAME, ACTION_CALL,' +
        ' ACTION_TYPE, '
      
        '  ACTION_CATEGORY, ACTION_CREATED, ACTION_LASTCALL, ACTION_CALLC' +
        'NT'
      'FROM ACTIONMANAGER_ACTION'
      'WHERE IDENT = LAST_INSERT_AUTOGEN()')
    ModifySQL.Strings = (
      'UPDATE ACTIONMANAGER_ACTION'
      
        'SET ACTION_NAME = :NEW_ACTION_NAME, ACTION_CALL = :NEW_ACTION_CA' +
        'LL, '
      
        '  ACTION_TYPE = :NEW_ACTION_TYPE, ACTION_CATEGORY = :NEW_ACTION_' +
        'CATEGORY, '
      
        '  ACTION_CREATED = :NEW_ACTION_CREATED, ACTION_LASTCALL = :NEW_A' +
        'CTION_LASTCALL, '
      '  ACTION_CALLCNT = :NEW_ACTION_CALLCNT'
      'WHERE IDENT = :OLD_IDENT;')
    DeleteSQL.Strings = (
      'DELETE FROM ACTIONMANAGER_ACTION'
      'WHERE IDENT = :OLD_IDENT')
    FetchRowSQL.Strings = (
      
        'SELECT LAST_INSERT_AUTOGEN() AS IDENT, ACTION_NAME, ACTION_CALL,' +
        ' ACTION_TYPE, '
      
        '  ACTION_CATEGORY, ACTION_CREATED, ACTION_LASTCALL, ACTION_CALLC' +
        'NT'
      'FROM ACTIONMANAGER_ACTION'
      'WHERE IDENT = :IDENT')
    Left = 200
    Top = 192
  end
  object FDTransaction1: TFDTransaction
    Connection = FDConnection1
    Left = 96
    Top = 160
  end
end
