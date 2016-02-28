from datetime import datetime
from datetime import timedelta

def timestamp_format(d):
    now = datetime.now()
    today = now.date()
    if d.date() == today or d > now - timedelta(hours=6):
        delta = datetime.now() - d
        if delta.seconds < 60:
            string = 'just now'
        elif delta.seconds < 3600:
            string = '%dmin ago' % (delta.seconds / 60)
        elif delta.seconds < 6 * 3600:
            string = '%dh ago' % (delta.seconds / 3600)
        else:
            string = d.strftime('%H:%M')
    elif d.date() == today - timedelta(1):
        string = d.strftime('yest %Hh')
    elif d.date() > today - timedelta(7):
        string = d.strftime('%a %Hh')
    elif d.year != today.year:
        string = d.strftime('%b %Y')
    else:
        string = d.strftime('%b %d')
    return string_decode(string, 'UTF-8')

# Copied from alot/helper.py
def string_decode(string, enc='ascii'):
    """
    safely decodes string to unicode bytestring, respecting `enc` as a hint.
    """

    if enc is None:
        enc = 'ascii'
    try:
        string = unicode(string, enc, errors='replace')
    except LookupError:  # malformed enc string
        string = string.decode('ascii', errors='replace')
    except TypeError:  # already unicode
        pass
    return string
