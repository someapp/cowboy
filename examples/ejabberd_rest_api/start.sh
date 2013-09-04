#!/bin/sh_



export SPARK_CONFIG_PATH=/opt/ejabberd-2.1.11/conf
POLL=true
SMP=auto
ERL_MAX_PORTS=32000
ERL_PROCESSES=250000
ERL_MAX_ETS_TABLES=1400

PATH="/opt/otp_src_R15B03/bin":$PATH
# define default environment variables 
ERL="/opt/otp_src_R15B03/bin/erl"
#ERL=erlexec
#DBG=erlexec
HERE=`which "$0" 2>/dev/null || echo .`
BASE=`dirname $HERE`
NODE=`hostname -f`
NAME=socialstream@${NODE}
COOKIE=CFHJKHWHACWYZQPBDHWS
ROOTDIR=`cd $BASE; pwd`
RUNDIR="${ROOTDIR}"
EBINDIR="${ROOTDIR}/ebin deps/*/ebin"
BIGWIGSASL=
SASL_LOG_PATH="$RUNDIR/logs/erlang.log"
MNESIA_DB="$RUNDIR/database"
DATETIME=`date "+%Y%m%d-%H%M%S"`
ERL_CRASH_DUMP="$RUNDIR/logs/erl_crash_$DATETIME.dump"
ERLANG_OPTS="+K $POLL -smp $SMP +P $ERL_PROCESSES $KERNEL_OPTS ${BIGWIGSASL}"

echo "Rootdir ${ROOTDIR}"
echo "Rundir ${RUNDIR}"
echo "Cookie ${COOKIE}"
echo "Name ${NAME}"
echo "Mnesia_db ${MNESIA_DB}"
echo "Sasl_log_path ${SASL_LOG_PATH}"

start()
{
   
    ulimit -n $ERL_MAX_PORTS 2>/dev/null
   
    erl -setcookie ${COOKIE} \
    	 ${ERLANG_OPTS} \
    	-name ${NAME} \
    	-pa ebin deps/*/ebin \
    	-boot start_sasl \
    	-s ejabberd_rest_api start -a -b 
    #	\
    #	-sasl sasl_error_logger \{file,\"$SASL_LOG_PATH\"\}
    
}


[ -d "${MNESIA_DB}" ] || mkdir -p "${MNESIA_DB}"

[ -d "${SASL_LOG_PATH}" ] || mkdir -p "${SASL_LOG_PATH}"


case $1 in
    start) start;;
esac
