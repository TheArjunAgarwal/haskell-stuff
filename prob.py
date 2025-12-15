def normal_win_chance_memo(points_req, a, b, p, dp):
    if dp[a][b] != -1:
        return dp[a][b]
    dp[a][b] = (
        p   * normal_win_chance_memo(points_req, a + 1, b,     p, dp)
      + (1-p) * normal_win_chance_memo(points_req, a,     b + 1, p, dp)
    )
    return dp[a][b]

def normal_win_chance(points_req, a, b, p):
    dp = [[-1]*(points_req+1) for _ in range(points_req+1)]

    for i in range(points_req+1):
        dp[points_req][i] = 1  # A has reached requirement → win
        dp[i][points_req] = 0  # B has reached requirement → lose

    return normal_win_chance_memo(points_req, a, b, p, dp)

def deuce_win_chance_memo(points_req, a, b, p, dp):
    if dp[a][b] != -1:
        return dp[a][b]
    dp[a][b] = (
        p   * normal_win_chance_memo(points_req, a + 1, b,     p, dp)
      + (1-p) * normal_win_chance_memo(points_req, a,     b + 1, p, dp)
    )
    return dp[a][b]

def deuce_win_chance(points_req, a, b, p):
    dp = [[-1]*(points_req+1) for _ in range(points_req+1)]
    q = 1 - p
    for i in range(points_req+1):
        dp[points_req][i] = 1  # A has reached requirement → win
        dp[i][points_req] = 0  # B has reached requirement → lose
    for i in range(points_req - 2, points_req + 1):
        dp[i][i] = p**2 / (1 - 2 * p * q)
    dp[points_req][points_req - 1] = p**3 / (1 - 2 * p * q)
    dp[points_req - 1][points_req - 2] = p**3 / (1 - 2 * p * q)
    
    dp[points_req - 1][points_req] = p + q * (p**2 / (1 - 2 * p * q))
    dp[points_req - 2][points_req - 1] = p + q * (p**2 / (1 - 2 * p * q))
    

    return deuce_win_chance_memo(points_req, a, b, p, dp)

def normal_win_matches_memo(points_req, a, b, p, dp):
    if dp[a][b] != -1:
        return dp[a][b]
    dp[a][b] = (
        p   * normal_win_matches_memo(points_req, a + 1, b,     p, dp)
      + (1-p) * normal_win_matches_memo(points_req, a,     b + 1, p, dp) + 1
    )
    return dp[a][b]

def normal_win_matches(points_req, a, b, p):
    dp = [[-1]*(points_req+1) for _ in range(points_req+1)]

    for i in range(points_req+1):
        dp[points_req][i] = 0  # A has reached requirement → win
        dp[i][points_req] = 0  # B has reached requirement → lose

    return normal_win_matches_memo(points_req, a, b, p, dp)

def deuce_win_matches_memo(points_req, a, b, p, dp):
    if dp[a][b] != -1:
        return dp[a][b]
    dp[a][b] = (
        1 + p   * normal_win_chance_memo(points_req, a + 1, b,     p, dp)
      + (1-p) * normal_win_chance_memo(points_req, a,     b + 1, p, dp) + 1
    )
    return dp[a][b]

def deuce_win_matches(points_req, a, b, p):
    dp = [[-1]*(points_req+1) for _ in range(points_req+1)]
    q = 1 - p
    for i in range(points_req+1):
        dp[points_req][i] = 0  # A has reached requirement → win
        dp[i][points_req] = 0  # B has reached requirement → lose
    x = 2 / (1 - 2 * p * q)
    for i in range(points_req - 2, points_req + 1):
        dp[i][i] = x
    y = p * x + 1
    z = q * x + 1
    dp[points_req][points_req - 1] = z
    dp[points_req - 1][points_req - 2] = z
    
    dp[points_req - 1][points_req] = y
    dp[points_req - 2][points_req - 1] = y
    
    return deuce_win_matches_memo(points_req, a, b, p, dp)

odds = 54
game_win_odds = deuce_win_chance(3, 0, 0, odds/100)
set_win_odds = deuce_win_chance(6, 0, 0, game_win_odds)
match_win_odds = normal_win_chance(3, 0, 0, set_win_odds)
game_rallies = deuce_win_matches(3,0,0,odds/100)
set_games = deuce_win_matches(6,0,0,game_win_odds)
match_sets = normal_win_matches(3,0,0, set_win_odds)
print(f"Odds of wining a game is {game_win_odds} which leads to the odds of winning a set to be {set_win_odds} which makes the match win odds {match_win_odds}")
print(f"The expected number of rallies per game is {game_rallies} and the game's per set is {set_games}; and {match_sets} are expected to be played. Thus in total, {game_rallies * set_games * match_sets} points will be played.")
