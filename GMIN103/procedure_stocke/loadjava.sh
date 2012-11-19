#! /bin/sh
#
# %W% %E%
#

unset JAVA_HOME
JAVA_HOME=/usr/local/oracle/client_1/jdk/jre
export JAVA_HOME

JRECLASSPATH=/usr/local/oracle/client_1/jdk/jre//lib/rt.jar:/usr/local/oracle/client_1/jdk/jre//lib/i18n.jar:/usr/local/oracle/client_1/jdbc/lib/ojdbc5.jar:/usr/local/oracle/client_1/sqlj/lib/translator.jar:/usr/local/oracle/client_1/javavm/lib/aurora.zip
export JRECLASSPATH

THREADS_FLAG=native
export THREADS_FLAG
LD_LIBRARY_PATH=$ORACLE_HOME/lib32:/usr/local/oracle/client_1/lib:$JAVA_HOME/lib
export LD_LIBRARY_PATH

#Need to pull passwords and usernames.
while [ $# -gt 0 ]
do
  if [ `echo $1|cut -c1-2` = "-u" ];  then
      LJUSER="$1"
      shift
      export LJUSER="$LJUSER $1";
  elif [ `echo $1|cut -c1-2` = "-P" ];  then
      LJPASS="-P"
      shift
      if [ "$1" = "" ]; then
          export LJPASS="$LJPASS \"\""
      else
          export LJPASS="$LJPASS $1"
      fi
  elif [ "$1" = "-password" ]; then
      LJPASS=$1
      shift
      if [ "$1" = "" ]; then
          export LJPASS="$LJPASS \"\""
      else
          export LJPASS="$LJPASS $1"
      fi
      export LJPASS="LJPASS $1"
  elif [ "$1" = "-resolver" ] || [ "$1" = "-R" ]; then
      RESOLVER="$1"
      shift
      export RESOLVER="$RESOLVER $1"
  else
      args="$args $1" 
  fi 
shift 
done

exec $JAVA_HOME/bin/java -classpath $JRECLASSPATH oracle.aurora.server.tools.loadjava.LoadJavaMain $args