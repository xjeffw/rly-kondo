(ns app.duplicate)

(defn ^{:global middle-fn} also-middle [x]
  x)

(defn ^:global twice [x]
  x)

(defn ^{:global twice} twice-again [x]
  x)
