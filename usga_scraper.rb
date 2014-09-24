require 'rubygems'
require 'nokogiri'
require 'rest-client'
require 'csv'

events = {
  "US Amateur" => 17179869326,
  "US Women's Amateur" => 17179869348,
  "Junior Amateur" => 17179869329,
  "Girls' Junior Amateur" => 17179869328,
  "Publinks" => 17179869204,
  "Women's Publinks" => 17179869344,
  "Mid-Amateur" => 17179869327,
  "Women's Mid-Amateur" => 17179869349,
  "Senior Amateur" => 17179869331,
  "Senior Women's Amateur" => 17179869332
}

new_format_years = [2014]
old_format_years = (2010..2013)

all_matches = events.each.with_object({}) do |(event_name, event_id), results_hsh|
  new_format_years.each do |year|
    tournament_results = {}
    
    (1..5).each do |round|
      round_url = "http://www.usga.org/ChampEventScoreDetail.aspx?id=#{event_id}&year=#{year}&type=match#{round}"
      round_matches = Nokogiri::HTML(RestClient.get(round_url))

      scoring_path = round_matches.css('#bd iframe').detect { |elm| elm["src"].to_s =~ /^\/scoring/i }["src"]
      scoring_data = Nokogiri::HTML(RestClient.get("http://www.usga.org#{scoring_path}"))
      puts scoring_path
      
      scoring_data.css("tr.scores").each do |match|
        summary = match.previous_element
        final_score = summary.css(".scoreCol").inner_text
        
        player_1 = match.css(".scorecardTbl tr")[3].css("td").map(&:inner_text)
        player_1.shift if player_1.length > 18
        
        player_2 = match.css(".scorecardTbl tr")[6].css("td").map(&:inner_text)
        
        scores = (0..18).map do |hole_number|
          next 0 if hole_number == 0

          ix = hole_number - 1
          p1 = player_1[ix]
          p2 = player_2[ix]

          if p1 == "AS"
            0
          elsif p1 =~ /\d+ up/i
            p1.split.first.to_i
          elsif p2 =~ /\d+ up/i
            -1 * p2.split.first.to_i
          end
        end
        
        raise "something funny with #{scoring_path}" if scores.last.to_i.abs > 2
        
        tournament_results[summary.inner_text] = {
          :final => final_score,
          :scores => scores
        }
      end
    end
    
    results_hsh[[event_name, year]] = tournament_results
  end
  
  old_format_years.each do |year|
    bracket_url = "http://www.usga.org/ChampEventScore.aspx?id=#{event_id}&year=#{year}&type=reversetree"
    bracket = Nokogiri::HTML(RestClient.get(bracket_url))

    match_links = bracket.css("#tblTree a").select { |e| e["href"] =~ /type=\d+$/ }

    tournament_results = match_links.each.with_object({}) do |match_link, hsh|
      puts match_link["href"]
      match = Nokogiri::HTML(RestClient.get("http://www.usga.org/#{match_link['href']}"))

      next if match.css(".scorecardTbl tr").any? { |elm| elm.inner_text.to_s.downcase == '36-hole final' }

      final_score = match.css(".matchScorecard table:not(.mainTbl)")[0].css("td").last.inner_text

      player_1 = match.css(".scorecardTbl tr.cRowMP")[0].css("td").map(&:inner_text)
      player_1.shift if player_1.length > 18

      player_2 = match.css(".scorecardTbl tr.cRowMP")[3].css("td").map(&:inner_text)

      scores = (0..18).map do |hole_number|
        next 0 if hole_number == 0
        
        ix = hole_number - 1
        p1 = player_1[ix]
        p2 = player_2[ix]

        if p1 == "AS"
          0
        elsif p1 =~ /\d+ up/i
          p1.split.first.to_i
        elsif p2 =~ /\d+ up/i
          -1 * p2.split.first.to_i
        end
      end

      raise "something funny with #{match_link['href']}" if scores.last.to_i.abs > 2

      hsh[match_link['href']] = {
        :final => final_score,
        :scores => scores
      }
    end
    
    results_hsh[[event_name, year]] = tournament_results
  end
end

CSV.open("usga_hole_by_hole_data.csv", "w") do |csv|
  csv << ["event", "year", "match_id", "final", "hole", "score", "next_score"]
  
  all_matches.each do |event_name, event_hsh|
    event_hsh.each do |match_url, match_hsh|
      (0..18).each do |hole|
        next if (match_score = match_hsh[:scores][hole]).nil?
        next_score = match_hsh[:scores][hole + 1]
        
        csv << [event_name, match_url, match_hsh[:final], hole, match_score, next_score].flatten
      end
    end
  end
end
