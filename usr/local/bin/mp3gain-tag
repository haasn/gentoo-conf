#!/bin/sh

LC_NUMERIC=POSIX; export LC_NUMERIC

MP3GAIN=`which mp3gain`
if [[ $? != 0 ]]; then
    echo "mp3gain executable not found." >&2
    exit 1
fi

EYED3=`which eyeD3`
if [[ $? != 0 ]]; then
    echo "eyeD3 executable not found." >&2
    exit 1
fi

if [[ $# > 1 || $# == 1 && $1 != "-f" ]] ; then
    echo "Usage: `basename $0` [-f]" >&2
    echo " for ReplayGain'ing all mp3 file into current directory" >&2
    echo " -f -- force re-ReplayGain'ing for already ReplayGain'ed files" >&2
    exit 2
fi

if [[ $# == 0 ]]; then
    $EYED3 --no-color *.mp3 \
    | egrep -i '^UserTextFrame: \[Description: replaygain_album_gain\]$' >/dev/null 2>&1
    if [[ $? == 0 ]]; then
        echo "Files already ReplayGain'ed." >&2
        exit 1
    fi
fi

$MP3GAIN *.mp3

TMPFILE=`mktemp`

for n in *.mp3; do
    $MP3GAIN -s c "$n" > $TMPFILE
    $MP3GAIN -s d "$n"
    TRACK_GAIN=`awk '/^Recommended "Track" dB / { printf("%+.2f dB", $5)   }' $TMPFILE`
    ALBUM_GAIN=`awk '/^Recommended "Album" dB / { printf("%+.2f dB", $5)   }' $TMPFILE`
    TRACK_PEAK=`awk '/^Max PCM /                { printf("%.6f", $7/32768) }' $TMPFILE`
    ALBUM_PEAK=`awk '/^Max Album PCM /          { printf("%.6f", $8/32768) }' $TMPFILE`
    $EYED3 \
        --set-user-text-frame="replaygain_track_gain:$TRACK_GAIN" \
        --set-user-text-frame="replaygain_track_peak:$TRACK_PEAK" \
        --set-user-text-frame="replaygain_album_gain:$ALBUM_GAIN" \
        --set-user-text-frame="replaygain_album_peak:$ALBUM_PEAK" \
        "$n"
done

rm -f $TMPFILE
