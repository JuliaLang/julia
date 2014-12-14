@test ip"127.0.0.1" == IPv4(127,0,0,1)
@test ip"192.0" == IPv4(192,0,0,0)
@test ip"192.0xFFF" == IPv4(192,0,15,255)
@test ip"192.0xFFFF" == IPv4(192,0,255,255)
@test ip"192.0xFFFFF" == IPv4(192,15,255,255)
@test ip"192.0xFFFFFF" == IPv4(192,255,255,255)
@test_throws ErrorException Base.parseipv4("192.0xFFFFFFF")
@test ip"022.0.0.1" == IPv4(18,0,0,1)

@test_throws ErrorException Base.parseipv4("192.0xFFFFFFFFF")
@test_throws ErrorException Base.parseipv4("192.")

@test ip"::1" == IPv6(1)
@test ip"2605:2700:0:3::4713:93e3" == IPv6(parseint(UInt128,"260527000000000300000000471393e3",16))

@test ip"2001:db8:0:0:0:0:2:1" == ip"2001:db8::2:1" == ip"2001:db8::0:2:1"

@test ip"0:0:0:0:0:ffff:127.0.0.1" == IPv6(0xffff7f000001)

# RFC 5952 Compliance

@test repr(ip"2001:db8:0:0:0:0:2:1") == "ip\"2001:db8::2:1\""
@test repr(ip"2001:0db8::0001") == "ip\"2001:db8::1\""
@test repr(ip"2001:db8::1:1:1:1:1") == "ip\"2001:db8:0:1:1:1:1:1\""
@test repr(ip"2001:db8:0:0:1:0:0:1") == "ip\"2001:db8::1:0:0:1\""
@test repr(ip"2001:0:0:1:0:0:0:1") == "ip\"2001:0:0:1::1\""

@test repr(IPv4Net("1.2.3.4","255.255.255.0")) == repr(IPv4Net("1.2.3.4/24")) == "IPv4Net(\"1.2.3.4\", \"255.255.255.0\")"
@test repr(IPv6Net("2001::1/20")) == "IPv6Net(\"2001::1/20\")"
@test in(IPv4("1.2.3.4"),IPv4Net("1.2.3.0/24"))
@test in(IPv4("1.2.4.4"),IPv4Net("1.2.3.0/24")) == false
@test contains(IPv4Net("1.2.3.0/24"), IPv4("1.2.3.4"))
@test contains(IPv4Net("1.2.3.0/24"), IPv4("1.2.4.4")) == false
@test_throws ErrorException IPv4Net("1.2.3.4/33")
@test_throws ErrorException IPv4Net("1.2.3.4/-1")
@test_throws ErrorException IPv4Net("1.2.3.4/20.5")
@test in(IPv6("2001:0001:0002::1"), IPv6Net("2001:0001:0002::/64"))
@test in(IPv6("2001:0001:0002::1"), IPv6Net("2001:0001:2222::/64")) == false
@test contains(IPv6Net("2001:0001:0002::/64"), IPv6("2001:0001:0002::1"))
@test contains(IPv6Net("2001:0001:2222::/64"), IPv6("2001:0001:0002::1")) == false
