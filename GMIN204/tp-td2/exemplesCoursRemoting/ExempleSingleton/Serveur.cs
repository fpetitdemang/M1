using System;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Tcp;
using System.Runtime.Remoting.Channels.Http;

namespace RemotingSamples {
  public class Sample {

    public static int Main(string [] args) {

      TcpChannel chan1 = new TcpChannel(8089);
      ChannelServices.RegisterChannel(chan1, true);
      RemotingConfiguration.RegisterWellKnownServiceType(
          typeof(HelloServer), "SayHello", WellKnownObjectMode.Singleton);
      System.Console.WriteLine("Appuyez sur <entre> pour sortir...");
      System.Console.ReadLine();
      return 0;
    }
  }
}