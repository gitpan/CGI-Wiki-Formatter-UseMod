package CGI::Wiki::Formatter::UseMod;

use strict;

use vars qw( $VERSION @_links_found );
$VERSION = '0.03';

use URI::Escape;
use Text::WikiFormat as => 'wikiformat';
use HTML::PullParser;
use URI::Find::Delimited;

=head1 NAME

CGI::Wiki::Formatter::UseMod - UseModWiki-style formatting for CGI::Wiki

=head1 DESCRIPTION

A formatter backend for L<CGI::Wiki> that supports UseMod-style formatting.

=head1 SYNOPSIS

  use CGI::Wiki::Formatter::UseMod;

  # Instantiate - see below for parameter details.
  my $formatter = CGI::Wiki::Formatter::UseMod->new( %config );

  # Format some text.
  my $cooked = $formatter->format($raw);

  # Find out which other nodes that text would link to.
  my @links_to = $formatter->find_internal_links($raw);

  # UseModWiki "encodes" node names before making them part of a URL, so
  # for example a node about Wombat Defenestration will have a URL like
  #   http://example.com/wiki.cgi?Wombat_Defenestration
  # So if we want to emulate a UseModWiki exactly, we need to munge back
  # and forth between node names as titles, and node names as CGI params.
  my $node_param = $q->param('id') || $q->param('keywords') || "";
  my $node_name = $formatter->node_param_to_node_name( $node_param );

  use URI::Escape;
  my $url = "http://example.com/wiki.cgi?"
    . uri_escape(
       $formatter->node_name_to_node_param( "Wombat Defenestration" )
                 );

  # Yes, this is a bit of a pain, transparent ways to do this solicited.

=head1 METHODS

=over 4

=item B<new>

  my $formatter = CGI::Wiki::Formatter::UseMod->new(
                 extended_links      => 0, # $FreeLinks
                 implicit_links      => 1, # $WikiLinks
                 force_ucfirst_nodes => 1, # $FreeUpper
                 use_headings        => 1, # $UseHeadings
                 allowed_tags        => [qw(b i)], # defaults to none
                 macros              => {},
	         node_prefix         => 'wiki.pl?',
                 edit_prefix         => 'wiki.pl?action=edit&id=' );

Parameters will default to the values shown above (apart from
C<allowed_tags>, which defaults to allowing no tags).

=over 4

=item B<Macros>

Be aware that macros are processed I<after> filtering out disallowed
HTML tags.  They are also not called in any particular order.

The keys of macros should be either regexes or strings. The values can
be strings, or, if the corresponding key is a regex, can be coderefs.
The coderef will be called with the first nine substrings captured by
the regex as arguments. I would like to call it with all captured
substrings but apparently this is complicated.

=back

Macro examples:

  macros => {

      '@SEARCHBOX' =>
 	        qq(<form action="wiki.pl" method="get">
                   <input type="hidden" name="action" value="search">
                   <input type="text" size="20" name="terms">
                   <input type="submit"></form>),

      qr/\@INDEX\s+\[Category\s+([^\]]+)]/ =>
            sub { return "{an index of things in category $_[0]}" }

  }

=cut

sub new {
    my ($class, @args) = @_;
    my $self = {};
    bless $self, $class;
    $self->_init(@args) or return undef;
    return $self;
}

sub _init {
    my ($self, %args) = @_;

    # Store the parameters or their defaults.
    my %defs = ( extended_links      => 0,
	         implicit_links      => 1,
                 force_ucfirst_nodes => 1,
                 use_headings        => 1,
		 allowed_tags        => [],
		 macros              => {},
	         node_prefix         => 'wiki.pl?',
                 edit_prefix         => 'wiki.pl?action=edit&id=',
	       );

    my %collated = (%defs, %args);
    foreach my $k (keys %defs) {
        $self->{"_".$k} = $collated{$k};
    }

    return $self;
}

=item B<format>

  my $html = $formatter->format($submitted_content, $wiki);

Escapes any tags which weren't specified as allowed on creation, then
interpolates any macros, then translates the raw Wiki language
supplied into HTML.

A L<CGI::Wiki> object can be supplied as an optional second parameter.
This object will be used to determine whether a linked-to node exists
or not, and alter the presentation of the link accordingly. This is
only really in here for use when this method is being called from
within L<CGI::Wiki>.

=cut

sub format {
    my ($self, $raw, $wiki) = @_;
    my $safe = "";

    my %allowed = map {lc($_) => 1, "/".lc($_) => 1} @{$self->{_allowed_tags}};

    # Parse the HTML - even if we're not allowing any tags, because we're
    # using a custom escaping routine rather than CGI.pm
    my $parser = HTML::PullParser->new(doc   => $raw,
				       start => '"TAG", tag, text',
				       end   => '"TAG", tag, text',
				       text  => '"TEXT", tag, text');
    while (my $token = $parser->get_token) {
        my ($flag, $tag, $text) = @$token;
	if ($flag eq "TAG" and !defined $allowed{lc($tag)}) {
	    $safe .= $self->_escape_HTML($text);
	} else {
	    $safe .= $text;
	}
    }

    # Now do any inline links.
    my $finder = URI::Find::Delimited->new( ignore_quoted => 1 );
    $finder->find(\$safe);

    # Now process any macros.
    my %macros = %{$self->{_macros}};
    foreach my $key (keys %macros) {
        my $value = $macros{$key};
        if ( ref $value && ref $value eq 'CODE' ) {
            $safe =~ s/$key/$value->($1, $2, $3, $4, $5, $6, $7, $8, $9)/eg;
	} else {
	  $safe =~ s/$key/$value/g;
	}
    }

    # Finally set up config and call Text::WikiFormat.
    my %format_opts = ( extended       => $self->{_extended_links},
			prefix         => $self->{_node_prefix},
			implicit_links => $self->{_implicit_links} );

    my %format_tags = (
        newline => "",
        extended_link_delimiters => [ '[[', ']]' ],
        lists                    => {
		         ordered         => qr/^\s*([\d]+)\.\s*/,
                         unordered       => qr/^\s*\*\s*/,
                         definition      => qr/^:\s*/
                                    },
        definition               => [ "<dl>\n", "</dl>\n", "<dt><dd>", "\n" ],
        link                     => sub {
            my ($link, $opts) = @_;
            $opts ||= {};

            my $title;
            ($link, $title) = split(/\|/, $link, 2) if $opts->{extended};
            $title =~ s/^\s*// if $title; # strip leading whitespace
            $title ||= $link;

            if ( $self->{_force_ucfirst_nodes} ) {
                $link = $self->_do_freeupper($link);
	    }
	    $link = $self->_munge_spaces($link);

            my $editlink_not_link = 0;
            # See whether the linked-to node exists, if we can.
            if ( $wiki && !$wiki->node_exists( $link ) ) {
                $editlink_not_link = 1;
	    }

	    $link =~ s/ /_/g;

            $link = uri_escape( $link );

            my $prefix = $opts->{prefix} || '';
            if ( $editlink_not_link ) {
                my $editprefix = $self->{_edit_prefix};
                return qq|[$title]<a href="$editprefix$link">?</a>|;
	    } else {
                return qq|<a href="$prefix$link">$title</a>|;
	    }
        },
    );

    # We need to be sneaky now, because we want to force the things
    # inside a heading to be formatted - but Text::WikiFormat won't do
    # this by default, since it only calls format_line on what remains
    # *after* the regex has been removed from the line, and our
    # header-recognising regex needs to encompass the entire line to
    # make sure that the = are balanced.
    #
    # We're OK with the 'options', since up there ---^ we provide a value
    # for each one.  Note that the defaulting of opts in Text::WikiFormat
    # is done in the format sub, rather than with a merge_hash with a tags
    # hash defined at the top of the package, so if more opts are
    # added to Text::WikiFormat we'll need to add them too.
    #
    # Need to merge the 'tags' though.

    my %all_tags = %Text::WikiFormat::tags;
    Text::WikiFormat::merge_hash( \%format_tags, \%all_tags );

    if ( $self->{_use_headings} ) {
        $format_tags{heading} = [ "", "",
            sub { "<h".(length $_[1]).">".Text::WikiFormat::format_line($_[2], \%all_tags, \%format_opts)."</h".(length $_[1]).">\n" } ];
#        $format_tags{lists}{heading} = qr/^(={1,6})\s+([^=]+)\s+\1\s*$/;
        $format_tags{lists}{heading} = qr/^(={1,6})\s+(.+)\s+\1\s*$/;
    }

    return wikiformat($safe, \%format_tags, \%format_opts );
}

# CGI.pm is sometimes awkward about actually performing CGI::escapeHTML
# if there's a previous instantiation - in the calling script, for example.
# So just do it here.
sub _escape_HTML {
    my ($self, $text) = @_;
    $text =~ s{&}{&amp;}gso;
    $text =~ s{<}{&lt;}gso;
    $text =~ s{>}{&gt;}gso;
    $text =~ s{"}{&quot;}gso;
    return $text;
}

=item B<find_internal_links> 
 
  my @links_to = $formatter->find_internal_links( $content ); 
 
Returns a list of all nodes that the supplied content links to. 
 
=cut 
 
sub find_internal_links { 
    my ($self, $raw) = @_;
 
    @_links_found = (); 
 
    my %format_opts = ( extended       => $self->{_extended_links},
			prefix         => $self->{_node_prefix},
			implicit_links => $self->{_implicit_links} );

    my %format_tags = ( extended_link_delimiters => [ '[[', ']]' ],
                        link => sub {
                            my ($link, $opts) = @_;
                            $opts ||= {};
                            my $title;
                            ($link, $title) = split(/\|/, $link, 2)
                              if $opts->{extended};
                            if ( $self->{_force_ucfirst_nodes} ) {
                                $link = $self->_do_freeupper($link);
	                    }
			    $link = $self->_munge_spaces($link);
                            push @CGI::Wiki::Formatter::UseMod::_links_found,
                                                                         $link;
                            return ""; # don't care about output
                                     }
    );

    my $foo = wikiformat($raw, \%format_tags, \%format_opts);

    my @links = @_links_found;
    @_links_found = ();
    return @links;
}


=item B<node_name_to_node_param>

  use URI::Escape;
  $param = $formatter->node_name_to_node_param( "Recent Changes" );
  my $url = "wiki.pl?" . uri_escape($param);

In usemod, the node name is encoded prior to being used as part of the URL.

=cut

sub node_name_to_node_param {
    my ($self, $node_name) = @_;
    my $param = $node_name;
    $param = $self->_munge_spaces($param);
    $param =~ s/ /_/g;

    return $param;
}

=item B<node_param_to_node_name>

  my $node = $q->param('node') || "";
  $node = $formatter->node_param_to_node_name( $node );

In usemod, the node name is encoded prior to being used as part of the
URL, so we must decode it before we can get back the original node name.

=cut

sub node_param_to_node_name {
    my ($self, $param) = @_;

    # Note that this might not give us back exactly what we started with,
    # since in the encoding we collapse and trim whitespace; but this is
    # how usemod does it (as of 0.92) and usemod is what we're emulating.
    $param =~ s/_/ /g;

    return $param;
}

sub _do_freeupper {
    my ($self, $node) = @_;

    # This is the FreeUpper usemod behaviour, slightly modified from
    # their regexp, as we need to do it before we check whether the
    # node exists ie before we substitute the spaces with underscores.
    $node = ucfirst($node);
    $node =~ s|([- _.,\(\)/])([a-z])|$1.uc($2)|ge;

    return $node;
}

sub _munge_spaces {
    my ($self, $node) = @_;

    # Yes, we really do only munge spaces, not all whitespace. This is
    # how usemod does it (as of 0.92).
    $node =~ s/ +/ /g;
    $node =~ s/^ //;
    $node =~ s/ $//;

    return $node
}

=head1 AUTHOR

Kake Pugh (kake@earth.li).

=head1 COPYRIGHT

     Copyright (C) 2003 Kake Pugh.  All Rights Reserved.

This module is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 CREDITS

The grubstreet team (L<http://grault.net/grubstreet/>) sent some very
helpful bug reports. A lot of the work of this module is done within
chromatic's module, L<Text::WikiFormat>.

=head1 CAVEATS

This doesn't yet support all of UseMod's formatting features and
options, by any means. (In particular, it doesn't cope with nested
lists, but I think this might be something I need to fix within
L<Text::WikiFormat>.) This really truly I<is> a 0.0* release. Please
send bug reports, omissions, patches, and stuff, to me at
C<kake@earth.li>.

See L<http://the.earth.li/~kake/cgi-bin/cgi-wiki/wiki.cgi> for a rough
stab at a wiki that emulates the functionality as well as the
formatting of a UseMod wiki. The source code is linked from there too.

=head1 SEE ALSO

=over 4

=item * L<CGI::Wiki>

=item * L<Text::WikiFormat>

=item * UseModWiki (L<http://www.usemod.com/cgi-bin/wiki.pl>)

=back

=cut

1;
