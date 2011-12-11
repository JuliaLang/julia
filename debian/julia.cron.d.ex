#
# Regular cron jobs for the julia package
#
0 4	* * *	root	[ -x /usr/bin/julia_maintenance ] && /usr/bin/julia_maintenance
