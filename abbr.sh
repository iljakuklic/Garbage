
function matches {
	case "$1" in
		$2) return 0 ;;
		*) return 1 ;;
	esac
}

function score {
	local POS=0
	local POSD=0
	local POSW=0
	local POSSW=0
	local PREV=''
	local SPACE VOWEL
	while read -s -N1 CHAR; do
		[ "$CHAR" == '' ] && CHAR=' '
		matches "$CHAR" "/" && POSD=0
		matches "$CHAR" "[[:space:]]" && SPACE=1 || SPACE=0
		matches "$PREV" "[ _/]" && POSW=0 && POSSW=0
		matches "$CHAR" "[[:upper:]]" && ! matches "$PREV" "[[:upper:]]" && POSSW=0
		matches "$CHAR" "[aeiouAEIOU]" && VOWEL=1 || VOWEL=0
		printf '%1d%1d%1d%1d%03d%1d%03d%03d:%03d:%s\n' "$SPACE" "$((!!POSD))" $((!!POSW)) $((!!POSSW)) \
			"$POSSW" "$VOWEL" "$POSW" "$POSD" "$POS" "$CHAR"
		(( POS++, POSD++, POSW++, POSSW++ ))
		PREV="$CHAR"
	done
}

function abbr {
	score | sort | head -${1:-5} | cut -d: -f2- | sort | cut -d: -f2- | sed 's/\n//g' | tr -d '\n'
	echo
}

for N in $(seq 1 "${#1}"); do
	echo "$1" | abbr $N
done

#score # | sort | head -$1 | cut -d: -f2- | sort
