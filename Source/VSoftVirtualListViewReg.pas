unit VSoftVirtualListViewReg;

interface

procedure Register;

implementation

uses
  System.Classes,
  VSoftVirtualListView;

procedure Register;
begin
  RegisterComponents('Virtual Controls', [TVSoftVirtualListView]);

end;


end.
