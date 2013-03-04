using System;
using System.Runtime.Remoting;

public class Server{

   public static void Main(){
      RemotingConfiguration.Configure("server.exe.config", true);
      Console.WriteLine("Waiting...");
      Console.ReadLine();
   }
}