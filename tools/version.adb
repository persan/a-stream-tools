with Stream_Tools;
with Ada.Text_IO;
with Ada.Command_Line;
procedure Version is
begin
   if $VERSION = Stream_Tools.Version then
      Ada.Text_IO.Put_Line (Stream_Tools.Version);
   else
      Ada.Text_IO.Put_Line ("Version mismatch (ProjectFile: " & $VERSION & ") /=  (Source: " &  Stream_Tools.Version & ").");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Version;
