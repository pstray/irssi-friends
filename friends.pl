#
# Copyright (C) 2001 by Peder Stray <peder@ninja.no>
#

use strict;
use Irssi;
use Irssi::Irc;
use Irssi::TextUI;
use Data::Dumper;

# ======[ Variables ]===================================================

my(%friends);

# ======[ Helper functions ]============================================

# --------[ crap ]------------------------------------------------------

sub crap {
    my $template = shift;
    my $msg = sprintf $template, @_;
    Irssi::printformat(MSGLEVEL_CLIENTCRAP, 'friends_crap', $msg);
}

# --------[ load_friends ]----------------------------------------------

sub load_friends {
    my($file) = Irssi::get_irssi_dir."/friends";
    my($count) = 0;
    local(*FILE);

    %friends = ();
    open FILE, "< $file";
    while (<FILE>) {
	my($mask,$net,$channel,@flags) = split;
	for (@flags) {
	    $friends{$mask}{lc $net}{lc $channel}{$_} = 1;
	}
    }
    close FILE;
    $count = keys %friends;

    crap("Loaded $count friends from $file");
}

# --------[ save_friends ]----------------------------------------------

sub save_friends {
    my($auto) = @_;
    my($file) = Irssi::get_irssi_dir."/friends";
    my($count) = 0;
    local(*FILE);

    return if $auto && !Irssi::settings_get_bool('keepnick_autosave');

    open FILE, "> $file";
    for my $mask (keys %friends) {
	$count++;
	for my $net (keys %{$friends{$mask}}) {
	    for my $channel (keys %{$friends{$mask}{$net}}) {
		print FILE "$mask\t$net\t$channel\t".
		  join(" ", sort keys %{$friends{$mask}{$net}{$channel}}).
		    "\n";
	    }
	}
    }
    close FILE;

    crap("Saved $count friends to $file")
      unless $auto;
}

# --------[ get_friends_window ]----------------------------------------

sub get_friends_window {
    my($win) = Irssi::window_find_name('<Friends>');
    if ($win) {
	$win->set_active;
    } else {
	Irssi::command("/window new hide");
	$win = Irssi::active_win;
	$win->set_name('<Friends>');
    }
    return $win;
}

# --------[ get_friend ]------------------------------------------------

sub get_friend {
    my($channel,$nick) = @_;
    my($server) = $channel->{server};
    my($chan) = lc $channel->{name};
    my($net) = lc $server->{chatnet};
    my($flags,@friend);

    for my $mask (keys %friends) {
	next unless $server->mask_match_address($mask,
						$nick->{nick},
						$nick->{host});
	for my $n ('*', $net) {
	    for my $c ('*', $chan) {
		if (exists $friends{$mask}{$n}{$c}) {
		    for my $flag (keys %{$friends{$mask}{$n}{$c}}) {
			$flags->{$flag} = 1;
		    }
		}
	    }
	}
	return $flags if $flags;
    }
    return undef;
}

# --------[ check_friends ]---------------------------------------------

sub check_friends {
    my($channel, @nicks) = @_;
    my(%op,%voice);
    my($nick,$friend,$list);
    my(@friends);

    for $nick (@nicks) {
	$friend = get_friend($channel, $nick);
	next unless $friend;
	next if $nick->{nick} eq $channel->{server}{nick};
	if ($friend->{op} && !$nick->{op}) {
	    $op{$nick->{nick}} = 1;
	}
	if ($friend->{voice} && !$nick->{voice}) {
	    $voice{$nick->{nick}} = 1;
	}
	push @friends, ($nick->{op}?'@':'').
	  ($nick->{voice}?'+':'').$nick->{nick};
    }

    $channel->printformat(MSGLEVEL_CLIENTCRAP, 'friends_check', "@friends")
      if @friends;

    if ($channel->{chanop}) {
	if ($list = join " ", sort keys %op) {
	    $channel->command("op $list");
	}
	if ($list = join " ", sort keys %voice) {
	    $channel->command("voice $list");
	}
    }
}

# ======[ Signal Hooks ]================================================

# --------[ sig_send_command ]------------------------------------------

sub sig_send_command {
    my($win) = Irssi::active_win;
    if ($win->{name} eq '<Friends>') {

	my($cmd,@param) = split " ", $_[0];
	$win->print("CMD: $cmd @param");

	if (lc $cmd eq 'exit') {
	    $win->print('exit in 5...');
	    $win->destroy;
	} else {
	    return;
	}

	Irssi::signal_stop;
    }
}

# --------[ sig_massjoin ]----------------------------------------------

sub sig_massjoin {
    my($channel, $nicks) = @_;
    check_friends($channel, @$nicks);
}

# --------[ sig_nick_mode_changed ]-------------------------------------

sub sig_nick_mode_changed {
    my($channel, $nick) = @_;
    if ($channel->{synced} &&
	$channel->{server}{nick} eq $nick->{nick} &&
	$nick->{op}) {
	check_friends($channel, $channel->nicks);
    }
}

# --------[ sig_channel_sync ]------------------------------------------

sub sig_channel_sync {
    my($channel) = @_;
    if ($channel->{chanop}) {
	check_friends($channel, $channel->nicks);
    }
}

# --------[ sig_setup_reread ]------------------------------------------

sub sig_setup_reread {
    load_friends;
}

# --------[ sig_setup_save ]--------------------------------------------

sub sig_setup_save {
    my($mainconf,$auto) = @_;
    save_friends($auto);
}

# ======[ Commands ]====================================================

# --------[ FRIENDS ]---------------------------------------------------

# Usage: /FRIENDS
sub cmd_friends {
    my($win) = get_friends_window;
    my($view) = $win->view;
    my($num) = 0;

    $view->remove_all_lines();
    $view->clear();
    for my $mask (sort keys %friends) {
	for my $net (sort keys %{$friends{$mask}}) {
	    for my $channel (sort keys %{$friends{$mask}{$net}}) {
		my $flags = join "", sort map { substr $_,0,1 }
		  keys %{$friends{$mask}{$net}{$channel}};
		Irssi::print(sprintf(
				     "[%%R%02d%%R] %-30s %-15s %-15s %s",
				     ++$num, $mask, $channel, $net, $flags
				    ), MSGLEVEL_NEVER);
	    }
	}
    }
}

# --------[ ADDFRIEND ]-------------------------------------------------

# Usage: /ADDFRIEND nick|mask flags [<channel>|* [<net>|*]]
#                                   [-host|-normal|-domain|-full]
sub cmd_addfriend {
    my($param,$serv,$chan) = @_;
    my(@param,@flags);
    my($type) = Irssi::Irc::MASK_USER | Irssi::Irc::MASK_DOMAIN;
    my($mask,$flags,$channel,$net);

    for (split " ", $param) {
	if (/^-h(ost)?$/) {
	    $type = Irssi::Irc::MASK_HOST;
	} elsif (/^-n(ormal)?$/) {
	    $type = Irssi::Irc::MASK_USER
		  | Irssi::Irc::MASK_DOMAIN;
	} elsif (/^-d(omain)?$/) {
	    $type = Irssi::Irc::MASK_DOMAIN;
	} elsif (/^-f(ull)?$/) {
	    $type = Irssi::Irc::MASK_NICK
		  | Irssi::Irc::MASK_USER
		  | Irssi::Irc::MASK_HOST;
	} else {
	    push @param, $_;
	}
    }
    ($mask,$flags,$channel,$net) = @param;

    unless ($mask) {
	crap("/ADDFRIEND [-full|-normal|-host|-domain] nick|mask [<[o][v]> [channel [chatnet]]]");
	return;
    }

    $flags ||= "o";

    unless ($channel) {
	if ($chan) {
	    $channel = $chan->{name};
	} else {
	    crap("/ADDFRIEND needs a channel.");
	    return;
	}
    }

    unless ($net) {
	if ($serv) {
	    $net = $serv->{chatnet};
	} else {
	    crap("/ADDFRIEND needs a chatnet.");
	    return;
	}
    }

    # is this a nick we need to expand?
    unless ($mask =~ /.+!.+@.+/) {
	my($nick);
	if ($net ne '*') {
	    unless ($serv = Irssi::server_find_chatnet($net)) {
		crap("Error locating server for $net.");
		return;
	    }
	} else {
	    unless ($serv) {
		crap("Need a server for nick expansion");
		return
	    }
	}
	if ($channel ne '*') {
	    unless ($chan = $serv->channel_find($channel)) {
		crap("Error locating channel $channel.");
		return;
	    }
	} else {
	    unless ($chan) {
		crap("Need a channel for nick expansion");
		return;
	    }
	}
	unless ($nick = $chan->nick_find($mask)) {
	    crap("Error locating nick $mask.");
	    return;
	}
	$mask = Irssi::Irc::get_mask($nick->{nick}, $nick->{host}, $type);
    }

    for my $flag (split //, $flags) {
	if ($flag eq 'o') {
	    $flag = 'op';
	} elsif ($flag eq 'v') {
	    $flag = 'voice';
	} else {
	    next;
	}
	push @flags, $flag;
	$friends{$mask}{lc $net}{lc $channel}{$flag} = 1;
    }

    if (@flags) {
	crap("Added %s for %s in %s on %s.",
	     join(",", @flags), $mask, $channel, $net);
    }

    save_friends(1);
}

# ======[ Setup ]=======================================================

# --------[ Register settings ]-----------------------------------------

Irssi::settings_add_bool('misc', 'friends_autosave', 1);

# --------[ Register formats ]------------------------------------------

Irssi::theme_register(
[
 'friends_crap',
 '{line_start}{hilight Friends:} $0',

 'friends_check',
 '{line_start}{hilight Friends} checked: $0',

]);

# --------[ Register signals ]------------------------------------------

Irssi::signal_add_first("send command", "sig_send_command");

Irssi::signal_add_last("massjoin", "sig_massjoin");
Irssi::signal_add_last("nick mode changed", "sig_nick_mode_changed");
Irssi::signal_add_last("channel sync", "sig_channel_sync");

Irssi::signal_add('setup saved', 'sig_setup_save');
Irssi::signal_add('setup reread', 'sig_setup_reread');

# --------[ Register commands ]-----------------------------------------

Irssi::command_bind('friends', 'cmd_friends');
Irssi::command_bind('addfriend', 'cmd_addfriend');

# --------[ Register timers ]-------------------------------------------

# --------[ Load config ]-----------------------------------------------

load_friends;

# ======[ END ]=========================================================

# Local Variables:
# header-initial-hide: t
# mode: header-minor
# end:
