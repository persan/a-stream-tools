package body Stream_Tools.Tests.BUG20220913.Eth.Messages.Command_Jak is

   overriding function Constructor (S : not null access Ada.Streams.Root_Stream_Type'Class) return Command_Jak_Message is
   begin
      raise Program_Error with "Cannot construct stp message";
      return (others => <>);
   end Constructor;

   procedure Write (S : not null access Ada.Streams.Root_Stream_Type'Class; Item : Command_Jak_Message) is
   begin
      Stream_Tools.Tests.BUG20220913_Low_Defs.Jp_List_Distribution_Acknowledge_T'Write (S, Item.Data);
   end Write;
begin
   Register (Message_Id => Id, Tag => Command_Jak_Message'Tag);

end Stream_Tools.Tests.BUG20220913.Eth.Messages.Command_Jak;
