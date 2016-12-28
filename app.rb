require 'roda'

class App < Roda
  use Rack::Session::Cookie, :secret => "COUCOU2"
  plugin :json, :classes=>[Array, Hash, String]

  route do |r|

    r.root do
      r.redirect "/hello"
    end
    
    r.on "hello" do

      r.post do
        "Martin Chabot"
      end
    end
  end
end 

