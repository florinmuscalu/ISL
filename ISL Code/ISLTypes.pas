unit ISLTypes;
interface
uses classes;
type
 TVarType=(Num, Str, Bool, nVect, sVect, bVect);

 TVariable = record
        fName: string[30];
        fType: TVarType;
        fValue: Variant;
 end;
 TArray=Array of Variant;
 TMethod=Function(Args:TArray; var error:pchar):Variant of object;
implementation

end.
