{ pkgs, config, ... }:

{

  xdg.configFile."khal/config".text = ''
    [calendars]

    [[calendars]]
    path = ~/.calendar/*
    type = discover

    [locale]
    timeformat = %H:%M
    dateformat = %Y-%m-%d
    longdateformat = %Y-%m-%d %a
    datetimeformat = %Y-%m-%d %H:%M
    longdatetimeformat = %Y-%m-%d %H:%M
  '';

  xdg.configFile."vdirsyncer/config".text = ''
    [general]
    status_path = "~/.config/vdirsyncer/status/"
    [pair calendars]
    a = "umass_local"
    b = "umass_remote"
    collections = ["calendar"]
    [storage umass_local]
    type = "filesystem"
    path = "~/.calendar/"
    fileext = ".ics"
    [storage umass_remote]
    type = "caldav"
    url = "http://localhost:1080/users/kandread@umass.edu/calendar/"
    username = "kandread@umass.edu"
    password.fetch = ["command", "cat", "/home/kandread/.cache/umass"]
    start_date = "datetime.now() - timedelta(days=365)"
    end_date = "datetime.now() + timedelta(days=365)"
  '';

}
