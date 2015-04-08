with Stream_Tools.Memory_Streams;
with Stream_Tools.Timed_Streams.Output;
procedure Stream_Tools.Examples.Simple_UDP is
   Real_Buffer : String (1 .. 1024) := (others => '#');
   S           : aliased Stream_Tools.Memory_Streams.Memory_Stream;
   Outf        : aliased Stream_Tools.Timed_Streams.Output.Timed_Output_Stream;
begin
   Outf.Create ("test.txt");
   S.Set_Address (Real_Buffer'Address);
   S.Set_Length (Real_Buffer'Length);

   String'Write (S'Access, "Funn");
   Integer'Write (S'Access, 123);

--    Memory_Streams.Memory_Stream'Write (Outf'Access, S);
   Memory_Streams.Write (Outf'Access, S);
   Outf.Close;
end Stream_Tools.Examples.Simple_UDP;
