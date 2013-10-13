# A collection of prolog projects 

Here you will find some experements in 'programmation en logique' aka [Prolog](https://en.wikipedia.org/wiki/Prolog).

All programs are written and tested in [SWIProlog](http://www.swi-prolog.org/)

## For sublimetext users

Here's my prolog build setup.
The idea is to invoking the goal main/0 after initializing.
Convinient for quick builds/tests, but you can not trace/debug your predicates, due to swipl immediatle shutting down after execution.
If someone has an idea how to nicely integrate the swipl console with sublimetext, please contact me ;)

`Prolog.sublime-build`

	{
		"cmd": 
			"swipl -q -f \"$file\" -g main"
	}

