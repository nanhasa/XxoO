namespace XxoODomain

module Rules =
    
    let rules = """
The game consists of 3x3 grid (A) with another 3x3 grid (B) inside of each of cell in grid A.

To win the game, player must form a line of three owned cells in grid A. To own grid A cell, player must form a line of three cells in the grid B of that cell in grid A. However, player can only input their mark in grid B which position on grid A matches the position of cell played by previous player in grid B. If the next grid B is already owned, player can choose where ever to put their mark.

Don't worry if this is confusing, the game will highlight next valid B grid(s) with green for you."""