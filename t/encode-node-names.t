use strict;
use CGI::Wiki::Formatter::UseMod;
use Test::More tests => 2;

my $formatter = CGI::Wiki::Formatter::UseMod->new;
is( $formatter->node_name_to_node_param( "test page" ),
    "Test_Page", "->node_name_to_node_param forces ucfirst by default" );

$formatter = CGI::Wiki::Formatter::UseMod->new( force_ucfirst_nodes => 0 );
is( $formatter->node_name_to_node_param( "test page" ),
    "test_page", "...but not if force_ucfirst_nodes set to 0" );

