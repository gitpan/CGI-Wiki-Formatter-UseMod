use strict;

use Test::More tests => 6;
use CGI::Wiki::Formatter::UseMod;

my $wikitext = <<WIKITEXT;

\@PLAIN_STRING

\@TEST_TEXT

\@INDEX_ALL

\@INDEX [Category Foo]

\@PAIR one two

WIKITEXT

my $formatter = CGI::Wiki::Formatter::UseMod->new(
    macros => {
        '@PLAIN_STRING' => "{plain string}",
        qr/\@TEST_TEXT(\b|$)/ => "{test text}",
        qr/\@INDEX_ALL(\b|$)/ => sub { return "{an index of all nodes}"; },
        qr/\@INDEX\s+\[Category\s+([^\]]+)]/ =>
            sub { return "{an index of things in category $_[0]}" },
        qr/\@PAIR\s+(\S*)\s+(\S*)(\b|$)/ =>
            sub { return "{" . join(" ", @_) . "}" }
    }
);
isa_ok( $formatter, "CGI::Wiki::Formatter::UseMod" );
my $html = $formatter->format($wikitext);

like( $html, qr|{plain string}|, "plain string macros work" );

like( $html, qr|{test text}|, "regex macros work" );

like( $html, qr|{an index of all nodes}|, "no-arg sub macros work" );

like( $html, qr|{an index of things in category Foo}|,
      "subs with a single arg work" );

SKIP: {
    skip "TODO - subs with more than one arg", 1;
    like( $html, qr|{one two}|, "subs with more than one arg work" );
}

