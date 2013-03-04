using System;
using System.Runtime.Remoting;
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Tcp;


namespace RemotingSamples
{
    public class Client
    {
        public static int Main(string[] args)
    {
        TcpChannel chan = new TcpChannel(8086);
        Console.WriteLine("channel cr��");
      ChannelServices.RegisterChannel(chan, true);
      Console.WriteLine("channel enregistr�");
      Console.ReadLine();
      ForwardMe param = new ForwardMe(); // objet cr�� chez le client
            // ligne suivante, on va chercher un proxy sur le HelloServer publi� chez le serveur sur le port 8085, nomm� SayHello
      HelloServer obj = (HelloServer)Activator.GetObject(typeof(RemotingSamples.HelloServer), "tcp://localhost:8085/SayHello");
      if (obj == null) System.Console.WriteLine("Impossible de trouver le serveur");
          // ligne suivante, on passe en param�tre le ForwardMe local au serveur
      else Console.WriteLine(obj.HelloMethod("Homme des cavernes",param));
      Console.Read();
      return 0;
    }
    }
}