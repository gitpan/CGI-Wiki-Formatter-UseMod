use strict;
use Test::More tests => 4;

use CGI::Wiki::Formatter::UseMod;

my $formatter = CGI::Wiki::Formatter::UseMod->new;

my $wikitext = <<WIKI;

* list item 1
* list item 2

 * another list item 1

* foo 1
** bar 1
** bar 2
* foo 2

WIKI

my $html = $formatter->format( $wikitext );
like( $html, qr/<li>list item 1<\/li>/, "unordered lists work" );
like( $html, qr/<li>another list item 1<\/li>/, "...also when indented" );
like( $html, qr/<li>foo 1/, "...first level of nested list" );
like( $html, qr/<ul>\s*<li>bar 1<\/li>/s, "...second level of nested list" );
